%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A sink process and writer for logi_sink_flow_limiter module
%% @private
%% @end
-module(logi_sink_flow_limiter_writer).

-behaviour(logi_sink_writer).
-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).

-export_type([start_arg/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/4, get_writee/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(REPORT_OMISSIONS_INTERVAL, (60 * 1000)).
-define(CHECK_WRITEE_INTERVAL, (1 * 1000)).

-define(STATE, ?MODULE).
-record(?STATE,
        {
          table                  :: ets:tid(),
          max_message_queue_len  :: non_neg_integer(),
          write_rate_limits      :: [logi_sink_flow_limiter:write_rate()],
          base_writer            :: logi_sink_writer:writer() | undefined,
          writee_status = normal :: writee_status()
        }).

-type start_arg() :: {logi:logger(), non_neg_integer(), [logi_sink_flow_limiter:write_rate()], logi_sink:sink()}.
-type writee_status() :: dead | queue_overflow | normal.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a new process
-spec start_link(start_arg()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Format, Data, {Table, BaseWriter}) ->
    case get_writee_status(Table) of
        dead           -> notify_omission(Table, writee_is_dead, Context);
        queue_overflow -> notify_omission(Table, message_queue_overflow, Context);
        normal         ->
            case is_rate_exceeded(Table) of
                true  -> notify_omission(Table, rate_exceeded, Context);
                false ->
                    WrittenData = logi_sink_writer:write(Context, Format, Data, BaseWriter),
                    ok = notify_write(Table, data_size(WrittenData)),
                    WrittenData
            end
    end.

%% @private
get_writee({_, BaseWriter}) ->
    logi_sink_writer:get_writee(BaseWriter).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init({Logger, MaxLen, WriteLimits, BaseSink}) ->
    _ = logi:save_as_default(Logger),
    case logi_sink_proc:start_child(BaseSink) of
        {error, Reason} -> {stop, Reason};
        {ok, SinkSup}   ->
            _ = link(SinkSup),
            Table = ets:new(?MODULE, [public, {write_concurrency, true}]),
            State0 =
                #?STATE{
                    table = Table,
                    max_message_queue_len = MaxLen,
                    write_rate_limits = WriteLimits
                   },
            true = ets:insert(Table, {total_write_bytes, 0, 0}),
            ok = schedule_check_writee(),
            ok = schedule_report_omissions(),
            ok = lists:foreach(fun (WriteRate) -> schedule_reset_write_bytes(WriteRate) end, WriteLimits),
            _ = logi:info("Started: base_sink=~p, max_message_queue_len=~p, write_rate_limits=~p",
                          [BaseSink, MaxLen, WriteLimits]),
            State1 = check_writee(State0),
            {ok, State1}
    end.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(check_writee, State0) ->
    State1 = check_writee(State0),
    ok = schedule_check_writee(),
    {noreply, State1};
handle_info({reset_write_bytes, WriteRate}, State) ->
    ok = reset_write_bytes(WriteRate, State),
    ok = schedule_reset_write_bytes(WriteRate),
    {noreply, State};
handle_info(report_omissions, State) ->
    ok = report_omissions(State),
    ok = schedule_report_omissions(),
    {noreply, State};
handle_info({sink_writer, _, Writer}, State) ->
    handle_sink_writer(Writer, State);
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(Reason, _State) ->
    _ = logi:info("Terminating: reason=~p", [Reason]),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec handle_sink_writer(logi_sink_writer:writer()|undefined, #?STATE{}) -> {noreply, #?STATE{}}.
handle_sink_writer(BaseWriter, State0) ->
    _ = logi:info("Base writer is updated: ~p", [BaseWriter]),
    Writer =
        case BaseWriter of
            undefined -> undefined;
            _         -> logi_sink_writer:new(?MODULE, {State0#?STATE.table, BaseWriter})
        end,
    ok = logi_sink_proc:send_writer_to_parent(Writer),
    State1 = State0#?STATE{base_writer = BaseWriter},
    State2 = check_writee(State1),
    {noreply, State2}.

-spec schedule_check_writee() -> ok.
schedule_check_writee() ->
    _ = erlang:send_after(?CHECK_WRITEE_INTERVAL, self(), check_writee),
    ok.

-spec schedule_report_omissions() -> ok.
schedule_report_omissions() ->
    _ = erlang:send_after(?REPORT_OMISSIONS_INTERVAL, self(), report_omissions),
    ok.

-spec schedule_reset_write_bytes(logi_sink_flow_limiter:write_rate()) -> ok.
schedule_reset_write_bytes(WriteRate = {_, Period}) ->
    _ = erlang:send_after(Period, self(), {reset_write_bytes, WriteRate}),
    ok.

%% TODO: rename
-spec reset_write_bytes(logi_sink_flow_limiter:write_rate(), #?STATE{}) -> ok.
reset_write_bytes(Rate, #?STATE{table = Table}) ->
    [{_, TotalWriteBytes, _}] = ets:lookup(Table, total_write_bytes),
    true = ets:insert(Table, {{write_bytes, Rate}, TotalWriteBytes}),
    NextMaxBytes =
        lists:min(
          [begin
               MaxBytesInPeriod + WriteBytesAtStartPeriod
           end || [MaxBytesInPeriod, WriteBytesAtStartPeriod] <- ets:match(Table, {{write_bytes, {'$1', '_'}}, '$2'})]),
    true = ets:update_element(Table, total_write_bytes, {3, NextMaxBytes}),
    ok.

-spec report_omissions(#?STATE{}) -> ok.
report_omissions(#?STATE{table = Table}) ->
    List = ets:match_object(Table, {{omissions, '_', '_', '_'}, '_'}),
    true = ets:match_delete(Table, {{omissions, '_', '_', '_'}, '_'}), % NOTE: 競合による若干の欠損は許容する
    lists:foreach(
      fun ({{omissions, Tag, Channel, Severity}, Count}) ->
              Location = logi_location:guess_location(),
              Sources = lists:append(ets:match(Table, {source, {Tag, Channel, Severity}, '$1'})),
              true = ets:match_delete(Table, {source, {Tag, Channel, Severity}, '_'}),

              logi:log(severity_max(warning, Severity),
                       "Over a period of ~p seconds, ~p ~s messages were omitted: channel=~s, reason=~s (e.g. ~p)",
                       [?REPORT_OMISSIONS_INTERVAL div 1000, Count, Severity, Channel, Tag,
                        [{pid,module,line} | Sources]],
                       [{location, Location}])
      end,
      List).

-spec severity_max(logi:severity(), logi:severity()) -> logi:severity().
severity_max(X, Y) ->
    lists:nth(min(logi:severity_level(X), logi:severity_level(Y)), logi:severities()).

-spec data_size(term()) -> non_neg_integer().
data_size(X) ->
    try
        iolist_size(X)
    catch
        _ -> erlang:external_size(X)
    end.

-spec notify_omission(ets:tid(), atom(), logi_context:context()) -> ok.
notify_omission(Id, Tag, Context) ->
    Channel = logi_context:get_channel(Context),
    Severity = logi_context:get_severity(Context),
    Key = {omissions, Tag, Channel, Severity},
    Count = ets:update_counter(Id, Key, {2, 1}, {Key, 0}),
    case Count < 5 of
        false -> ok;
        true  ->
            Location = logi_context:get_location(Context),
            Source =
                {logi_location:get_process(Location),
                 logi_location:get_module(Location),
                 logi_location:get_line(Location)},
            _ = ets:insert(Id, {source, {Tag, Channel, Severity}, Source}),
            ok
    end.

-spec notify_write(ets:tid(), non_neg_integer()) -> ok.
notify_write(Id, MessageSize) ->
    _ = ets:update_counter(Id, total_write_bytes, {2, MessageSize}),
    ok.

-spec get_alive_writee(#?STATE{}) -> pid() | undefined.
get_alive_writee(State) ->
    case State#?STATE.base_writer of
        undefined -> undefined;
        Writer    ->
            case logi_sink_writer:get_writee(Writer) of
                undefined -> undefined;
                Writee    ->
                    case is_process_alive(Writee) of
                        false -> undefined;
                        true  -> Writee
                    end
            end
    end.

-spec check_writee(#?STATE{}) -> #?STATE{}.
check_writee(State) ->
    Status =
        case get_alive_writee(State) of
            undefined -> dead;
            Writee    ->
                {_, Len} = process_info(Writee, message_queue_len),
                case Len > State#?STATE.max_message_queue_len of
                    true  -> queue_overflow;
                    false -> normal
                end
        end,
    case Status =:= State#?STATE.writee_status of
        true  -> State;
        false ->
            true = ets:insert(State#?STATE.table, {writee_status, Status}),
            State#?STATE{writee_status = Status}
    end.

-spec get_writee_status(ets:tid()) -> writee_status().
get_writee_status(Table) ->
    [{_, Status}] = ets:lookup(Table, writee_status),
    Status.

-spec is_rate_exceeded(ets:tid()) -> boolean().
is_rate_exceeded(Id) ->
    [{_, TotalWriteBytes, MaxBytes}] = ets:lookup(Id, total_write_bytes),
    TotalWriteBytes > MaxBytes.
