%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A sink which provides HA (High Availability) functionality
%% @end
-module(logi_sink_ha).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/2, new/3]).

-export_type([options/0, option/0]).
-export_type([select_strategy/0]).
-export_type([restart_strategy/0]).
-export_type([peer/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type options() :: [option()].

-type option() :: {logger, logi:logger()}
                | {strategy, select_strategy()}.
%% `logger':
%% - The logger instance which is used to report internal events of the sink process
%% - Default: `logi:default_logger()'
%%
%% `strategy':
%% - Peer selection strategy
%% - Default: `random'

-type select_strategy() :: first_available | random.
%% Peer selection strategy:
%% - `first_available': Selects first available peer in the list
%% - `random': Selects random peer in the list

-type restart_strategy() :: #{interval => timeout() | {Min :: timeout(), Max :: timeout()}}.
%% Exited peer restarting strategy

-type peer() ::
        #{
           sink    => logi_sink:sink(),
           restart => restart_strategy()
         }.
%% A peer specification
%%
%% `restart' is optional field (default value is `#{interval => {1000, 60000}}').

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(SinkId, Peers, [])
-spec new(logi_sink:id(), [peer()]) -> logi_sink:sink().
new(SinkId, Peers) ->
    new(SinkId, Peers, []).

-spec new(logi_sink:id(), [peer()], options()) -> logi_sink:sink().
new(SinkId, Peers, Options) ->
    Args = [SinkId, Peers, Options],
    _ = is_list(Options) orelse error(badarg, Args),

    Logger = proplists:get_value(logger, Options, logi:default_logger()),
    Strategy = proplists:get_value(strategy, Options, random),
    _ = logi:is_logger(Logger) orelse error(badarg, Args),
    _ = lists:member(Strategy, [first_available, random]) orelse error(badarg, Args),

    StartArg = {lists:map(fun normalize_peer/1, Peers), Logger, Strategy},
    logi_sink:new(#{id => SinkId, start => {logi_sink_ha_writer, start_link, [StartArg]}}).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec normalize_peer(peer()) -> peer().
normalize_peer(Peer) ->
    #{sink := _, restart := _} =
        maps:with([sink, restart], maps:merge(#{restart => #{interval => {1000, 60 * 1000}}}, Peer)).
