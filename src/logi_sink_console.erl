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
-export([install/1, install/2]).
-export([uninstall/0, uninstall/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv install(Condition, [])
-spec install(logi_sink:condition()) -> logi_channel:install_sink_result().
install(Condition) -> install(Condition, []).

%% @doc Installs a sink
%%
%% The default value of `Options':
%% - id: `logi_sink_console'
%% - channel: `logi_channel:default_channel()'
%% - layout: `logi_layout_color:new(logi_builtin_layout_simple:new())' TODO: logi_layout_default
-spec install(logi_sink:condition(), Options) -> logi_channel:install_sink_result() when
      Options :: [Option],
      Option  :: {id, logi_sink:id()}
               | {channel, logi_channel:id()}
               | {layout, logi_layout:layout()}
               | logi_channel:install_sink_option().
install(Condition, Options) ->
    SinkId = proplists:get_value(id, Options, ?MODULE),
    Channel = proplists:get_value(channel, Options, logi_channel:default_channel()),
    Layout = proplists:get_value(layout, Options, logi_layout_color:new(logi_builtin_layout_simple:new())),
    Colored = proplists:get_value(colored, Options, true),
    _ = logi_layout:is_layout(Layout) orelse error(badarg, [Condition, Options]),
    _ = is_boolean(Colored) orelse error(badarg, [Condition, Options]),

    Sink = logi_sink:new(SinkId, ?MODULE, Condition, Layout),
    logi_channel:install_sink(Channel, Sink, Options).

%% @equiv uninstall([])
-spec uninstall() -> logi_channel:uninstall_sink_result().
uninstall() -> uninstall([]).

%% @doc Uninstalls a sink
%%
%% The default value of `Options':
%% - id: `logi_sink_console'
%% - channel: `logi_channel:default_channel()'
-spec uninstall(Options) -> logi_channel:uninstall_sink_result() when
      Options :: [Option],
      Option  :: {id, logi_sink:id()}
               | {channel, logi_channel:id()}.
uninstall(Options) ->
    SinkId = proplists:get_value(id, Options, ?MODULE),
    Channel = proplists:get_value(channel, Options, logi_channel:default_channel()),
    logi_channel:uninstall_sink(Channel, SinkId).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Format, Data, Layout) ->
    IoData = logi_layout:format(Context, Format, Data, Layout),
    io:put_chars(user, IoData).
