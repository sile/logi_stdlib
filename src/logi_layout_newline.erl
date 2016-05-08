%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A logi_layout implementation which appends newline character(s) to tail of output messages
%% @end
-module(logi_layout_newline).

-behaviour(logi_layout).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).
-export_type([style/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([format/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type style() :: lf | cr | crlf.
%% Newline Style

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(BaseLayout, lf)
-spec new(logi_layout:layout()) -> logi_layout:layout().
new(BaseLayout) ->
    new(BaseLayout, lf).

%% @doc Creates a new layout instance
-spec new(logi_layout:layout(), style()) -> logi_layout:layout().
new(BaseLayout, Style) ->
    _ = logi_layout:is_layout(BaseLayout) orelse error(badarg, [BaseLayout, Style]),
    _ = lists:member(Style, [lf, cr, crlf]) orelse error(badarg, [BaseLayout, Style]),
    case Style of
        lf -> logi_layout:new(?MODULE, BaseLayout);
        _  -> logi_layout:new(?MODULE, {BaseLayout, Style})
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
format(Context, Format, Data, {Layout, cr})   -> [logi_layout:format(Context, Format, Data, Layout), $\r];
format(Context, Format, Data, {Layout, crlf}) -> [logi_layout:format(Context, Format, Data, Layout), $\r, $\n];
format(Context, Format, Data, Layout)         -> [logi_layout:format(Context, Format, Data, Layout), $\n].
