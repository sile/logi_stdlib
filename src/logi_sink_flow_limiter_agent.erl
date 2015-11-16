%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc メッセージの流量制御用シンクのサーバ側モジュール
%% @private
%%
%% TODO: 全体的に整理
-module(logi_sink_flow_limiter_agent).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/6]).
-export([notify_omission/3]).
-export([notify_write/2]).
-export([get_destination_status/1]).
-export([is_rate_exceeded/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records
%%----------------------------------------------------------------------------------------------------------------------
-define(OMISSION_REPORT_INTERVAL, (60 * 1000)).
-define(DESTINATION_CHECK_INTERVAL, (1 * 1000)).
-define(STATE, ?MODULE).

-record(?STATE,
        {
          table                 :: ets:tid(),
          max_message_queue_len :: non_neg_integer(),
          write_rate_limits     :: [logi_sink_flow_limiter:write_rate()],
          base_agent_pid        :: pid()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a new server
-spec start_link(pid(), logi_lib_proc:otp_name() | undefined, logi:logger(), non_neg_integer(),
                 [logi_sink_flow_limiter:write_rate()], logi_sink:spec()) ->
                        {ok, pid(), {pid(), ets:tid(), logi_sink:sink()}} | {error, Reason::term()}.
start_link(AgentSup, Name, Logger, MaxLen, WriteLimits, BaseSinkSpec) ->
    ReplyTag = make_ref(),
    Args = [self(), AgentSup, Logger, MaxLen, WriteLimits, BaseSinkSpec, ReplyTag],
    Result =
        case Name of
            undefined -> gen_server:start_link(?MODULE, Args, []);
            _         -> gen_server:start_link(Name, ?MODULE, Args, [])
        end,
    receive
        {ReplyTag, Table, BaseSink} ->
            case Result of
                {ok, Pid} -> {ok, Pid, {Pid, Table, BaseSink}};
                Other     -> Other
            end
    after 0 ->
            {error, _} = Result
    end.

-spec notify_omission(ets:tid(), atom(), logi_context:context()) -> ok.
notify_omission(Id, Tag, Context) ->
    Channel = logi_context:get_channel(Context),
    Severity = logi_context:get_severity(Context),
    Key = {omissions, Tag, Channel, Severity},
    Count = ets:update_counter(Id, Key, {2, 1}, {Key, 0}), % OTP-18 only
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

%% TODO: rename
-spec get_destination_status(ets:tid()) -> normal | dead | queue_overflow.
get_destination_status(Id) ->
    [{_, Status}] = ets:lookup(Id, destination_status),
    Status.

-spec is_rate_exceeded(ets:tid()) -> boolean().
is_rate_exceeded(Id) ->
    [{_, TotalWriteBytes, MaxBytes}] = ets:lookup(Id, total_write_bytes),
    TotalWriteBytes > MaxBytes.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([ParentPid, AgentSup, Logger, MaxLen, WriteLimits, BaseSinkSpec, ReplyTag]) ->
    _ = logi:save_as_default(Logger),
    case logi_agent_sup:start_agent(AgentSup, logi_sink:get_agent_spec(BaseSinkSpec)) of
        {error, Reason}                   -> {stop, Reason};
        {ok, BaseAgentCtrlPid, ExtraData} ->
            _ = monitor(process, BaseAgentCtrlPid),
            BaseSink = logi_sink:instantiate(BaseSinkSpec, ExtraData),
            Table = ets:new(?MODULE, [public, {write_concurrency, true}]),
            State =
                #?STATE{
                    table = Table,
                    max_message_queue_len = MaxLen,
                    write_rate_limits = WriteLimits,
                    base_agent_pid = (logi_sink:get_module(BaseSink)):whereis_agent(ExtraData)
                   },
            true = ets:insert(Table, {total_write_bytes, 0, 0}),
            ok = check_destination(State),
            ok = schedule_destination_check(),
            ok = schedule_omission_report(),
            ok = lists:foreach(fun (WriteRate) -> schedule_reset_write_bytes(WriteRate) end, WriteLimits),
            _ = logi:info("Started: base_sink=~p, max_message_queue_len=~p, write_rate_limits=~p",
                          [BaseSink, MaxLen, WriteLimits]),
            _ = ParentPid ! {ReplyTag, Table, BaseSink},
            {ok, State}
    end.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(destination_check, State) ->
    ok = check_destination(State),
    ok = schedule_destination_check(),
    {noreply, State};
handle_info({reset_write_bytes, WriteRate}, State) ->
    ok = reset_write_bytes(WriteRate, State),
    ok = schedule_reset_write_bytes(WriteRate),
    {noreply, State};
handle_info(omission_report, State) ->
    ok = report_omissions(State),
    ok = schedule_omission_report(),
    {noreply, State};
handle_info({'AGNET_DOWN', _, Pid, Reason}, State) ->
    _ = logi:warning("The base agent is down: pid=~p, reason=~p", [Pid, Reason]),
    {stop, Reason, State};
handle_info({'DOWN', _, _, Pid, Reason}, State) ->
    _ = logi:warning("The base agent is terminated: pid=~p, reason=~p", [Pid, Reason]),
    {stop, Reason, State};
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
-spec schedule_destination_check() -> ok.
schedule_destination_check() ->
    _ = erlang:send_after(?DESTINATION_CHECK_INTERVAL, self(), destination_check),
    ok.

-spec schedule_omission_report() -> ok.
schedule_omission_report() ->
    _ = erlang:send_after(?OMISSION_REPORT_INTERVAL, self(), omission_report),
    ok.

-spec schedule_reset_write_bytes(logi_sink_flow_limiter:write_rate()) -> ok.
schedule_reset_write_bytes(WriteRate = {_, Period}) ->
    _ = erlang:send_after(Period, self(), {reset_write_bytes, WriteRate}),
    ok.

%% TODO: rename: destination
-spec check_destination(#?STATE{}) -> ok.
check_destination(#?STATE{table = Table, base_agent_pid = Pid, max_message_queue_len = MaxLen}) ->
    Status =
        case erlang:process_info(Pid, message_queue_len) of
            {_, Len} when Len > MaxLen -> queue_overflow;
            {_, _}                     -> normal;
            _                          -> dead
        end,
    true = ets:insert(Table, {destination_status, Status}),
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
                       [?OMISSION_REPORT_INTERVAL div 1000, Count, Severity, Channel, Tag,
                        [{pid,module,line} | Sources]],
                       [{location, Location}])
      end,
      List).

-spec severity_max(logi:severity(), logi:severity()) -> logi:severity().
severity_max(X, Y) ->
    lists:nth(min(logi:severity_level(X), logi:severity_level(Y)), logi:severities()).
