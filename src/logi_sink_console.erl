%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Console sink for logi
%%
%% The sink outputs log messages to the user console.
%%
%% == NOTE ==
%%
%% TODO: no overload protection (only development purposes are recommended. see: lifetime option)
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% TODO
%% </pre>
-module(logi_sink_console).

-behaviour(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/5, default_layout/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new sink instance
%%
%% The default layout of the sink is `logi_layout_color:new(logi_layout_limit:new(logi_layout_default:new()))'.
-spec new() -> logi_sink:sink().
new() ->
    logi_sink:new(?MODULE).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Layout, Format, Data, _) ->
    IoData = logi_layout:format(Context, Format, Data, Layout),
    io:put_chars(user, IoData).

%% @private
default_layout(_) ->
    logi_layout_color:new(logi_layout_limit:new(logi_layout_default:new())).
