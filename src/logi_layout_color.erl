%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO: doc
-module(logi_layout_color).

-behaviour(logi_layout).

-export([new/1, new/2]).
-export([default_color/1]).
-export([format/4]).

-export_type([color_fun/0]).

-type color_fun() :: fun ((logi_context:context()) -> iodata()). % TODO: more doc

new(BaseLayout) ->
    new(BaseLayout, fun ?MODULE:default_color/1).

new(BaseLayout, Color) ->
    _ = logi_layout:is_layout(BaseLayout) orelse error(badarg, [BaseLayout, Color]),
    _ = is_function(Color, 1) orelse error(badarg, [BaseLayout, Color]),
    logi_layout:new(?MODULE, {BaseLayout, Color}).

default_color(Context) ->
    case logi_context:get_severity(Context) of
        debug     -> "\e[0m";
        verbose   -> "\e[0m";
        info      -> "\e[1m";
        notice    -> "\e[1;35m";
        warning   -> "\e[1;33m";
        error     -> "\e[1;31m";
        critical  -> "\e[1;31m";
        alert     -> "\e[1;7;31m";
        emergency -> "\e[1;7;31m"
    end.

format(Context, Format, Data, {Layout, Color}) ->
    [Color(Context), logi_layout:format(Context, Format, Data, Layout), "\e[0m"].
