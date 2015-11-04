%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO: doc
%%
%% TODO: anonymous function (= non full quialified function) is not recommended
-module(logi_layout_color).

-behaviour(logi_layout).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).
-export([default_color/1]).

-export_type([color_fun/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([format/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type color_fun() :: fun ((logi_context:context()) -> iodata()). % TODO: more doc

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(BaseLayout, fun logi_layout_color:default_color/1)
-spec new(logi_layout:layout()) -> logi_layout:layout().
new(BaseLayout) -> new(BaseLayout, fun ?MODULE:default_color/1).

-spec new(logi_layout:layout(), color_fun()) -> logi_layout:layout().
new(BaseLayout, Color) ->
    _ = logi_layout:is_layout(BaseLayout) orelse error(badarg, [BaseLayout, Color]),
    _ = is_function(Color, 1) orelse error(badarg, [BaseLayout, Color]),
    logi_layout:new(?MODULE, {BaseLayout, Color}).

-spec default_color(logi_context:context()) -> iodata().
default_color(Context) ->
    case logi_context:get_severity(Context) of
        debug     -> "\e[0m";
        info      -> "\e[1m";
        notice    -> "\e[1;35m";
        warning   -> "\e[1;33m";
        error     -> "\e[1;31m";
        critical  -> "\e[1;31m";
        alert     -> "\e[1;7;31m";
        emergency -> "\e[1;7;31m"
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
format(Context, Format, Data, {Layout, Color}) ->
    FormattedData = logi_layout:format(Context, Format, Data, Layout),
    [Color(Context), FormattedData, "\e[0m"].
    %% TODO: delete: [Color(Context), re:replace(FormattedData, "(\r|\n|)$", "\e[0m\\1")].
