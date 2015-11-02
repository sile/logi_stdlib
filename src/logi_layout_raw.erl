% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO: doc
%%
%% TODO: make more composable
-module(logi_layout_raw).

-behaviour(logi_layout).

-export([new/0]).
-export([format/4]).

new() ->
    logi_layout:new(?MODULE).

format(_Context, _Format, Data, _) ->
    Data.
