%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc 高可用性(HA, High Availability)を達成するための手段を提供するシンク
%%
%% 機能:
%% - Active/StandBy構成による多重化
%% - Active/Active構成による多重化
%% - 異常終了したプロセスのバックオフ付きの自動再起動
%%
%% memo:
%% - 監視ツリーは別に存在するけど、そっちのsupervisorが諦めた場合に手助けをするスタンス
%% - HA起動時の自動起動や、HA停止時の自動停止は行わない
%%
%% == EXAMPLE ==
%% TODO
-module(logi_sink_ha).

-behaviour(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_manager/2, start_manager/3]).
-export([stop_manager/1]).
-export([which_managers/0]).

-export([new/1]).

-export_type([manager_id/0]).
-export_type([options/0, option/0]).
-export_type([mode/0]).
-export_type([destination/0]).
-export_type([mfargs/0, restart_strategy/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type manager_id() :: atom().
%% The identifier of a HA manager

-type options() :: [option()].

-type option() :: {logger, logi:logger()}
                | {mode, mode()}.

-type mode() :: active_passive | active_active.

-type destination() :: %% TODO: name
        #{
           id         => atom(),
           start      => mfargs(),
           restart    => restart_strategy(),
           sink       => logi_sink:sink(),
           monitor_id => atom() % TODO: pid(), {via, _, _}, etc
         }.

-type mfargs() :: {module(), atom(), [term()]}.

-type restart_strategy() :: {exponential_backoff, Min::non_neg_integer(), Max::non_neg_integer()}
                          | {constant, Time::non_neg_integer()}.

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/3]).
-export([whereis_agent/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv start_manager(ManagerId, Destinations, [])
-spec start_manager(manager_id(), [destination()]) -> {ok, pid()} | {error, Reason::term()}.
start_manager(ManagerId, Destinations) ->
    start_manager(ManagerId, Destinations, []).

-spec start_manager(manager_id(), [destination()], options()) -> {ok, pid()} | {error, Reason::term()}.
start_manager(ManagerId, Destinations, Options) ->
    Args = [ManagerId, Destinations, Options],
    _ = is_atom(ManagerId) orelse error(badarg, Args),
    _ = is_list(Destinations) andalso length(Destinations) > 0 andalso lists:all(fun is_destination/1, Destinations) orelse error(badarg, Args),
    _ = is_list(Options) orelse error(badarg, Args),

    Logger = proplists:get_value(logger, Options, logi:default_logger()),
    Mode = proplists:get_value(mode, Options, active_active),
    _ = logi:is_logger(Logger) orelse error(badarg, Args),
    _ = lists:member(Mode, [active_passive, active_active]) orelse error(badarg, Args),
    _ = Mode =:= active_passive andalso error(not_implemented, Args), % TODO:

    logi_sink_ha_manager_sup:start_child(ManagerId, {Destinations, Logger, Mode}).

%% @doc Stops the manager
%%
%% If the manager associated to `ManagerId' does not exists, it is silently ignored.
-spec stop_manager(manager_id()) -> ok.
stop_manager(ManagerId) ->
    logi_sink_ha_manager_sup:stop_child(ManagerId).

%% @doc Returns a list of the running managers
-spec which_managers() -> [manager_id()].
which_managers() ->
    [Id || {Id, _} <- logi_sink_ha_manager_sup:which_children()].

%% @doc Creates a new sink instance
%%
%% The default layout is TODO
-spec new(manager_id()) -> logi_sink:sink().
new(ManagerId) ->
    _ = is_atom(ManagerId) orelse error(badarg, [ManagerId]),
    logi_sink:new(?MODULE, logi_builtin_layout_pass_through:new(), ManagerId).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, {Format, Data}, ManagerId) ->
    logi_sink_ha_manager:write_to_available_destination(ManagerId, Context, Format, Data).

%% @private
whereis_agent(ManagerId) ->
    whereis(ManagerId).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec is_destination(destination() | term()) -> boolean().
is_destination(Map = #{id := Id, start := Start, sink := Sink, monitor_id := MonitorId}) ->
    %% TODO: `Id'のユニーク性保証
    %% TODO: merge defaults
    Restart = maps:get(restart, Map, {exponential_backoff, 1000, 60 * 1000}),
    (is_atom(Id) andalso
     is_mfargs(Start) andalso
     logi_sink:is_sink(Sink) andalso
     is_restart_strategy(Restart) andalso
     is_atom(MonitorId));
is_destination(_) ->
    false.

-spec is_mfargs(mfargs() | term()) -> boolean().
is_mfargs({Module, Function, Args}) -> is_atom(Module) andalso is_atom(Function) andalso is_list(Args);
is_mfargs(_)                        -> false.

-spec is_restart_strategy(restart_strategy() | term()) -> boolean().
is_restart_strategy({exponential_backoff, Min, Max}) -> is_integer(Min) andalso Min >= 0 andalso is_integer(Max) andalso Max > 1;
is_restart_strategy({constant, Time})                -> is_integer(Time) andalso Time >= 0;
is_restart_strategy(_)                               -> false.
