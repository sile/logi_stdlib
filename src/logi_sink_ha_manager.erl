%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc 高可用性(HA, High Availability)を達成するための手段を提供するシンクのマネージャプロセス
%%
%% @private
-module(logi_sink_ha_manager).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/2]).
-export([get_default_layout/1]).
-export([write_to_available_destination/5]).

-export_type([start_arg/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(OMISSION_REPORT_INTERVAL, (60 * 1000)).
-define(STATE, ?MODULE).

-record(?STATE,
        {
          table                 :: ets:tid(),
          mode                  :: logi_sink_ha:mode(),
          destinations          :: #{Id::atom() => logi_sink_ha:destination()},
          monitor_to_sink = #{} :: #{reference() => {Id::atom(), Index::non_neg_integer()}}
        }).

-type start_arg() :: {[logi_sink_ha:destination()], logi:logger(), logi_sink_ha:mode()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a new manager process
-spec start_link(logi_sink_ha:manager_id(), start_arg()) -> {ok, pid()} | {error, Reason::term()}.
start_link(ManagerId, Arg) ->
    gen_server:start_link({local, ManagerId}, ?MODULE, [ManagerId, Arg], []).

%% @doc Gets the default layout
-spec get_default_layout(logi_sink_ha:manager_id()) -> logi_layout:layout().
get_default_layout(ManagerId) ->
    gen_server:call(ManagerId, get_default_layout).

%% @doc 利用可能な宛先にログメッセージを書き込む
-spec write_to_available_destination(
        logi_sink_ha:manager_id(), logi_context:context(), logi_layout:layout(), io:format(), logi_layout:data()) -> any().
write_to_available_destination(ManagerId, Context, Layout, Format, Data) ->
    case select_sink(ManagerId) of
        error      -> notify_omission(ManagerId, no_available_destination, Context);
        {ok, Sink} -> logi_sink:write(Context, Layout, Format, Data, Sink)
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([ManagerId, {Destinations, Logger, Mode}]) ->
    _ = logi:save_as_default(Logger),
    _ = logi:set_headers(#{id => ManagerId}),
    Table = ets:new(ManagerId, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
    State0 =
        #?STATE{
            table        = Table,
            mode         = Mode,
            destinations = maps:from_list([{Id, maps:merge(#{restart => {exponential_backoff, 1000, 60 * 1000}}, D)} || D = #{id := Id} <- Destinations])
           },
    true = ets:insert(Table, {counter, 0}),
    true = ets:insert(Table, {availables, 0}),

    %% TODO: Supports active/passive
    State1 = lists:foldl(fun register_destination/2, State0, Destinations),
    ok = schedule_omission_report(),
    _ = logi:info("Started: mode=~s, destinations=~p", [Mode, Destinations]),
    {ok, State1}.

%% @private
handle_call(get_default_layout, _From, State) ->
    Layout = logi_sink:default_layout(maps:get(sink, hd(maps:values(State#?STATE.destinations)))),
    {reply, Layout, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(omission_report, State) ->
    ok = report_omissions(State),
    ok = schedule_omission_report(),
    {noreply, State};
handle_info({restart, Id, WaitedTime}, State) ->
    handle_restart(Id, WaitedTime, State);
handle_info({'DOWN', Monitor, _, Pid, Reason}, State) ->
    handle_down(Monitor, Pid, Reason, State);
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
-spec select_sink(logi_sink_ha:manager_id()) -> {ok, logi_sink:sink()} | error.
select_sink(Id) ->
    Count = ets:update_counter(Id, counter, {2, 1}),
    [{_, Limit}] = ets:lookup(Id, availables),
    case Limit of
        0 -> error;
        _ ->
            I = Count rem Limit,
            case ets:lookup(Id, {available, I}) of
                []          -> select_sink(Id);
                [{_, Sink}] -> {ok, Sink}
            end
    end.

-spec notify_omission(logi_sink_ha:manager_id(), atom(), logi_context:context()) -> ok.
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

-spec schedule_omission_report() -> ok.
schedule_omission_report() ->
    _ = erlang:send_after(?OMISSION_REPORT_INTERVAL, self(), omission_report),
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
                       [?OMISSION_REPORT_INTERVAL div 1000, Count, Severity, Channel, Tag, [{pid,module,line} | Sources]],
                       [{location, Location}])
      end,
      List).

-spec severity_max(logi:severity(), logi:severity()) -> logi:severity().
severity_max(X, Y) ->
    lists:nth(min(logi:severity_level(X), logi:severity_level(Y)), logi:severities()).

-spec handle_down(reference(), pid(), term(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_down(Monitor, Pid, Reason, State0) ->
    {Id, _} = maps:get(Monitor, State0#?STATE.monitor_to_sink),
    _ = logi:critical("The destination '~s' is down: pid=~p, reason=~p", [Id, Pid, Reason]),
    State1 = deregister_destination(Monitor, State0),
    ok = schedule_destination_restart(Id, 0, State1),
    {noreply, State1}.

-spec schedule_destination_restart(atom(), non_neg_integer(), #?STATE{}) -> ok.
schedule_destination_restart(Id, PrevTime, State) ->
    #{restart := Restart} = maps:get(Id, State#?STATE.destinations),
    RestartTime =
        case Restart of
            {exponential_backoff, Min, Max} ->
                %% TODO: 起動に成功した場合でも`Time'間は値を覚えておいて、その間に失敗したら続きからやる、とかは必要そう
                CurTime = PrevTime * 2,
                max(Min, min(Max, CurTime));
            {constant, Time} ->
                Time
        end,
    _ = logi:info("Tries restarting the destination '~s' after ~p milliseconds", [Id, RestartTime]),
    _ = erlang:send_after(RestartTime, self(), {restart, Id, RestartTime}),
    ok.

-spec handle_restart(atom(), non_neg_integer(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_restart(Id, WaitedTime, State0) ->
    D = #{start := {M, F, Args}, monitor_id := MonitorId} = maps:get(Id, State0#?STATE.destinations),
    _ = logi:info("Restarting the destination '~s': mfargs=~p", [Id, {M, F, Args}]),
    case (case whereis(MonitorId) of undefined -> (catch apply(M, F, Args)); Pid0 -> {ok, Pid0} end) of % XXX:
        {ok, Pid} ->
            _ = logi:notice("Restarting the destination '~s' is succeeded: pid=~p", [Id, Pid]),
            State1 = register_destination(D, State0),
            {noreply, State1};
        Reason ->
            _ = logi:notice("Restarting the destination '~s' is failed: reason=~p", [Id, Reason]),
            ok = schedule_destination_restart(Id, WaitedTime, State0),
            {noreply, State0}
    end.

-spec register_destination(logi_sink_ha:destination(), #?STATE{}) -> #?STATE{}.
register_destination(#{id := Id, sink := Sink, monitor_id := MonitorId}, State = #?STATE{table = Table}) ->
    Monitor = monitor(process, MonitorId),
    [{_, Next}] = ets:lookup(Table, availables),
    true = ets:insert(Table, {{available, Next}, Sink}),
    true = ets:insert(Table, {availables, Next + 1}),
    _ = logi:info("The destination '~s' is registered: index=~p, availables=~p", [Id, Next, Next + 1]),
    MonitorToSink = maps:put(Monitor, {Id, Next}, State#?STATE.monitor_to_sink),
    State#?STATE{monitor_to_sink = MonitorToSink}.

-spec deregister_destination(reference(), #?STATE{}) -> #?STATE{}.
deregister_destination(Monitor, State = #?STATE{table = Table}) ->
    {Id, Index} = maps:get(Monitor, State#?STATE.monitor_to_sink),

    [{_, Next}] = ets:lookup(Table, availables),
    Last = Next - 1,
    [{_, Sink}] = ets:lookup(Table, {available, Last}),
    true = ets:update_element(Table, {available, Index}, {2, Sink}),
    true = ets:insert(Table, {availables, Last}),
    true = ets:delete(Table, {available, Last}),
    _ = logi:info("The destination '~s' is deregistered: index=~p, availables=~p", [Id, Index, Last]),

    MonitorToSink =
        maps:map(
          fun (_, V = {_, I}) ->
                  case I =:= Last of
                      false -> V;
                      true  ->
                          _ = logi:info("The index of the destination '~s' is changed: ~p => ~p",
                                        [element(1, V), I, Index]),
                          setelement(2, V, Index)
                  end
          end,
          maps:remove(Monitor, State#?STATE.monitor_to_sink)),
    State#?STATE{monitor_to_sink = MonitorToSink}.
