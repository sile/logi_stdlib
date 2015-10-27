%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO: doc
%% @private
-module(logi_sink_flow_limiter_server).

-compile({parse_transform, logi_transform}).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/2]).
-export([notify_omission/3]).
-export([notify_write/2]).
-export([get_write_bytes/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records
%%----------------------------------------------------------------------------------------------------------------------
-define(OMISSION_REPORT_INTERVAL, 60 * 1000). % TODO: option

-define(STATE, ?MODULE).

-record(?STATE,
        {
          table  :: ets:tid(),
          period :: timeout() % TODO: 複数サポート: [{period, 1000, bits, 1024 * 1024 * 8}, {period, 60 * 60 * 1000, bits, 100 * 1024 * 1024 * 8}]
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec start_link(logi_sink_flow_limiter:id(), logi_sink_flow_limiter:options()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Id, Options) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id, Options], []).

-spec notify_omission(logi_sink_flow_limiter:id(), atom(), logi_context:context()) -> ok.
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

-spec notify_write(logi_sink_flow_limiter:id(), non_neg_integer()) -> ok.
notify_write(Id, MessageSize) ->
    _ = ets:update_counter(Id, write_bytes, {2, MessageSize}, {write_bytes, 0}),
    ok.

-spec get_write_bytes(logi_sink_flow_limiter:id()) -> non_neg_integer().
get_write_bytes(Id) ->
    case ets:lookup(Id, write_bytes) of
        [{_, Size}] -> Size;
        []          -> 0
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([Id, Options]) ->
    Table = ets:new(Id, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
    State =
        #?STATE{
            table = Table,
            period = proplists:get_value(period, Options, 1000) % XXX
           },
    ok = schedule_omission_report(),
    ok = schedule_reset_write_bytes(State),
    {ok, State}.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(reset_write_bytes, State) ->
    ok = reset_write_bytes(State),
    ok = schedule_reset_write_bytes(State),
    {noreply, State};
handle_info(log_omission_report, State) ->
    ok = report_omissions(State),
    ok = schedule_omission_report(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec schedule_omission_report() -> ok.
schedule_omission_report() ->
    _ = erlang:send_after(?OMISSION_REPORT_INTERVAL, self(), log_omission_report),
    ok.

-spec schedule_reset_write_bytes(#?STATE{}) -> ok.
schedule_reset_write_bytes(#?STATE{period = Period}) ->
    _ = erlang:send_after(Period, self(), reset_write_bytes),
    ok.

-spec reset_write_bytes(#?STATE{}) -> ok.
reset_write_bytes(#?STATE{table = Table}) ->
    true = ets:delete(Table, write_bytes),
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

              logi:log(Severity, "Over a period of ~p seconds, ~p ~s messages were omitted: reason=~s (e.g. ~p)",
                       [?OMISSION_REPORT_INTERVAL div 1000, Count, Severity, Tag, [{pid,module,line} | Sources]],
                       [{location, Location},
                        {metadata, #{urgent => true}},
                        {logger, logi:new([{channel, Channel}])}])
      end,
      List).
