%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi_restart_strategy_backoff).

-behaviour(logi_restart_strategy).

-export([new/0, new/1]).

-export([next/1]).

-spec new() -> logi_restart_strategy:strategy().
new() ->
    new({1000, 60 * 1000}).

-spec new({timeout(), timeout()}) -> logi_restart_strategy:strategy().
new({Min, Max}) ->
    logi_restart_strategy:new(?MODULE, {Min, Max, Min}).


%% TODO: reset if succeeded
next({Min, Max, Cur}) ->
    Next = min(Max, Cur * 2),
    {ok, Cur, {Min, Max, Next}}.
