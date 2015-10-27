%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO: logi_sink_tcp
%%
%% @doc TODO
-module(logi_sink_file).

-behaviour(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_writer/2, stop_writer/1]).

-export([install/1, install/2]).
-export([uninstall/0, uninstall/1]).

-export_type([writer_id/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type writer_id() :: atom().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% TODO: doc
-spec start_writer(writer_id(), logi_sink_file_path:path()) -> {ok, MayChange::pid()} | {error, Reason::term()}.
start_writer(WriterId, PathGen) ->
    logi_sink_file_writer_sup:start_child(WriterId, PathGen).

-spec stop_writer(writer_id()) -> ok.
stop_writer(WriterId) ->
    logi_sink_file_writer_sup:stop_child(WriterId).

%% @equiv install(Condition, [])
-spec install(logi_sink:condition()) -> logi_channel:install_sink_result().
install(Condition) -> install(Condition, []).

%% @doc Installs a sink
%%
%% The default value of `Options':
%% - id: `logi_sink_file'
%% - channel: `logi_channel:default_channel()'
%% - layout: `logi_layout_color:new(logi_builtin_layout_simple:new())' TODO: logi_layout_default
-spec install(logi_sink:condition(), Options) -> logi_channel:install_sink_result() when
      Options :: [Option],
      Option  :: {id, logi_sink:id()}
               | {channel, logi_channel:id()}
               | {layout, logi_layout:layout()}
               | {writer, writer_id()}
               | logi_channel:install_sink_option().
install(Condition, Options) ->
    SinkId = proplists:get_value(id, Options, ?MODULE),
    Channel = proplists:get_value(channel, Options, logi_channel:default_channel()),
    Layout = proplists:get_value(layout, Options, logi_layout_color:new(logi_layout_default:new())), % TODO: delete color
    Writer = proplists:get_value(writer, Options, {error, not_found}),
    _ = logi_layout:is_layout(Layout) orelse error(badarg, [Condition, Options]),
    _ = is_atom(Writer) orelse error(badarg, [Condition, Options]),

    Sink = logi_sink:new(SinkId, ?MODULE, Condition, {Layout, Writer}),
    logi_channel:install_sink(Channel, Sink, Options).

%% @equiv uninstall([])
-spec uninstall() -> logi_channel:uninstall_sink_result().
uninstall() -> uninstall([]).

%% @doc Uninstalls a sink
%%
%% The default value of `Options':
%% - id: `logi_sink_file'
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
write(Context, Format, Data, {Layout, Writer}) ->
    IoData = logi_layout:format(Context, Format, Data, Layout),
    logi_sink_file_writer:write(Writer, IoData).
