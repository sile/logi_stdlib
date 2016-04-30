%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%% @doc TODO
%% @end
-module(logi_sink_ha_writer).

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
-record(peer,
        {
          id              :: logi_sink:id(),
          sink            :: logi_sink:sink(),
          restart         :: logi_sink_ha:restart_strategy(),
          writer          :: logi_sink_writer:writer() | undefined,
          child_id        :: logi_sink_proc:child_id() | undefined,
          retry_count = 0 :: non_neg_integer(),
          started_at      :: erlang:timestamp() | undefined
        }).

-define(STATE, ?MODULE).
-record(?STATE,
        {
          table      :: ets:tid(),
          peers = [] :: [#peer{}],
          strategy   :: logi_sink_ha:select_strategy()
        }).

-type start_arg() :: {[logi_sink_ha:peer()], logi:logger(), logi_sink_ha:select_strategy()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec start_link(start_arg()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Format, Data, Table) ->
    case select_writer(Table) of
        error        -> []; %% TODO: report omission and notify stopped
        {ok, Writer} -> logi_sink_writer:write(Context, Format, Data, Writer)
    end.

%% @private
get_writee(_) ->
    undefined.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init({Peers, Logger, Strategy}) ->
    _ = logi:save_as_default(Logger),
    Table = ets:new(?MODULE, [protected, {read_concurrency, true}]),
    State0 =
        #?STATE{
            table    = Table,
            strategy = Strategy
           },
    true = ets:insert(Table, {availables, 0}),
    State1 = lists:foldl(fun start_peer/2, State0,
                         [#peer{id = logi_sink:get_id(S), sink = S, restart = R} || #{sink := S, restart := R} <- Peers]),
    %% TODO: ok = schedule_omission_report(),
    {ok, State1}.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({restart, Arg}, State) ->
    handle_restart(Arg, State);
handle_info({'DOWN', _, _, Pid, Reason}, State) ->
    _ = logi:error("Writer ~p is down: reason=~p", [Pid, Reason]), % TODO: ログは全体的に見直し
    handle_down(Pid, State);
handle_info({sink_writer, ChildId, Writer}, State) ->
    handle_sink_writer(ChildId, Writer, State);
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
-spec start_peer(#peer{}, #?STATE{}) -> #?STATE{}.
start_peer(Peer0, State) ->
    Peer1 =
        case logi_sink_proc:start_child(Peer0#peer.sink) of
            {error, Reason} ->
                _ = logi:error("Can't start sink: id=~s, reason=~p", [Peer0#peer.id, Reason]),
                ok = schedule_restart(Peer0),
                Peer0#peer{started_at = undefined};
            {ok, ChildId} ->
                _ = monitor(process, ChildId),
                Peer0#peer{child_id = ChildId, started_at = erlang:timestamp()}
        end,
    Peers = lists:keystore(Peer0#peer.id, #peer.id, State#?STATE.peers, Peer1),
    State#?STATE{peers = Peers}.

-spec handle_down(logi_sink_proc:child_id(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_down(ChildId, State0) ->
    Peer0 = #peer{} = lists:keyfind(ChildId, #peer.child_id, State0#?STATE.peers),
    Peer1 =
        Peer0#peer{
          child_id = undefined,
          writer = undefined,
          started_at = undefined,
          retry_count = 0 % TODO: 最後の起動時刻からの経過時間を測定して、ゼロリセットするかどうかを決定する
         },
    Peers = lists:keystore(Peer1#peer.id, #peer.id, State0#?STATE.peers, Peer1),
    State1 = State0#?STATE{peers = Peers},
    _ = Peer0#peer.writer =/= undefined andalso update_table(State1),
    ok = schedule_restart(Peer1),
    {noreply, State1}.

-spec handle_restart(logi_sink:id(), #?STATE{}) -> {noreply, #?STATE{}}.
handle_restart(Id, State0) ->
    Peer0 = #peer{} = lists:keyfind(Id, #peer.id, State0#?STATE.peers),
    Peer1 = Peer0#peer{retry_count = Peer0#peer.retry_count + 1},
    State1 = start_peer(Peer1, State0),
    {noreply, State1}.

-spec handle_sink_writer(logi_sink_proc:child_id(), logi_sink_writer:writer() | undefined, #?STATE{}) -> {noreply, #?STATE{}}.
handle_sink_writer(ChildId, Writer, State0) ->
    Peer0 = #peer{} = lists:keyfind(ChildId, #peer.child_id, State0#?STATE.peers),
    case Peer0#peer.writer =:= Writer of
        true  -> {noreply, State0};
        false ->
            Peer1 = Peer0#peer{writer = Writer},
            Peers = lists:keystore(Peer1#peer.id, #peer.id, State0#?STATE.peers, Peer1),
            State1 = State0#?STATE{peers = Peers},
            ok = update_table(State1),
            ok = case {length(get_availables(State0)), length(get_availables(State1))} of
                     {_, 0} -> logi_sink_proc:send_writer_to_parent(undefined);
                     {0, 1} -> logi_sink_proc:send_writer_to_parent(logi_sink_writer:new(?MODULE, State1#?STATE.table));
                     _      -> ok
                 end,
            {noreply, State1}
    end.

-spec schedule_restart(#peer{}) -> ok.
schedule_restart(Peer) ->
    After =
        case Peer#peer.restart of
            #{interval := {Min, Max}} ->
                min(Max, round(Min * math:pow(2, Peer#peer.retry_count)));
            #{interval := Interval} ->
                Interval
        end,
    _ = erlang:send_after(After, self(), {restart, Peer#peer.id}),
    ok.

-spec get_availables(#?STATE{}) -> [#peer{}].
get_availables(#?STATE{peers = Peers, strategy = Strategy}) ->
    case lists:filter(fun (#peer{writer = Writer}) -> Writer =/= undefined end, Peers) of
        [X | _] when Strategy =:= first_available -> [X];
        Availables                                -> Availables
    end.

-spec update_table(#?STATE{}) -> ok.
update_table(State) ->
    Availables = get_availables(State),
    Count = length(Availables),
    true = ets:insert(
             State#?STATE.table,
             [
              {availables, Count} |
              [{{writer, I}, Writer} || {I, #peer{writer = Writer}} <- lists:zip(lists:seq(1, Count), Availables)]
             ]),
    ok = lists:foreach(
           fun (I) -> ets:delete(State#?STATE.table, {writer, I}) end,
           lists:seq(Count + 1, length(State#?STATE.peers))),
    ok.

-spec select_writer(ets:tid()) -> {ok, logi_sink_writer:writer()} | error.
select_writer(Table) ->
    [{_, Availables}] = ets:lookup(Table, availables),
    case Availables of
        0 -> error;
        _ ->
            I = case Availables of
                    1 -> 1;
                    _ -> rand:uniform(Availables)
                end,
            case ets:lookup(Table, {writer, I}) of
                []            -> select_writer(Table);
                [{_, Writer}] -> {ok, Writer}
            end
    end.
