%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%% @end
%%
%% 高可用性(HA, High Availability)を達成するための手段を提供するシンク
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


-type select_strategy() :: first_available | random.

%% TODO: backoffの場合は「N秒生きていたらリセットする」的な値も必要
-type restart_strategy() :: #{interval => non_neg_integer() | {non_neg_integer(), non_neg_integer()}}.

-type peer() ::
        #{
           sink => logi_sink:sink(),
           restart => restart_strategy() % optional
         }.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec new(logi_sink:id(), [peer()]) -> logi_sink:sink().
new(Id, Peers) ->
    new(Id, Peers, []).

-spec new(logi_sink:id(), [peer()], options()) -> logi_sink:sink().
new(Id, Peers, Options) ->
    Logger = proplists:get_value(logger, Options, logi:default_logger()),
    Strategy = proplists:get_value(strategy, Options, random),
    StartArg = {lists:map(fun normalize_peer/1, Peers), Logger, Strategy},
    logi_sink:new(#{id => Id, start => {logi_sink_ha_writer, start_link, [StartArg]}}).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec normalize_peer(peer()) -> peer().
normalize_peer(Peer) ->
    #{sink := _, restart := _} = maps:with([sink, restart], maps:merge(#{restart => {1000, 60 * 1000}}, Peer)).
