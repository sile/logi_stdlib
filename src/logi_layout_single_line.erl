%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A logi_layout implementation which removes newline characters from output messages
%% @end
-module(logi_layout_single_line).

-behaviour(logi_layout).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([format/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new layout instance
-spec new(logi_layout:layout()) -> logi_layout:layout().
new(BaseLayout) ->
    _ = logi_layout:is_layout(BaseLayout) orelse error(badarg, [BaseLayout]),
    logi_layout:new(?MODULE, BaseLayout).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
format(Context, Format, Data, Layout) ->
    FormattedData = logi_layout:format(Context, Format, Data, Layout),
    re:replace(FormattedData, "(\r|\n|\r\n)\\s*", "", [global, multiline]).
