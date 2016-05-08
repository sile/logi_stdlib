%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A sink which prints log messages to the console
%%
%% == NOTE==
%%
%% The sink has no overload protections,
%% so it is recommended to use it together with (for example) {@link logi_slink_flow_limiter}
%% in a production environment.
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > error_logger:tty(false). % Suppresses annoying warnings for the sake of brevity
%% >
%% > logi_channel:install_sink(logi_sink_console:new(foo), info).
%% > logi:info("hello world").
%% 2015-11-03 10:58:59.920 [info] nonode@nohost &lt;0.113.0&gt; erl_eval:do_apply:673 [] hello world
%% </pre>
%%
%% Uses other layout:
%% <pre lang="erlang">
%% > logi_channel:install_sink(logi_sink_console:new(foo, logi_layout_io_lib_format:new()), info).
%% > logi:info("hello world").
%% hello world
%% </pre>
%% @end
-module(logi_sink_console).

-behaviour(logi_sink_writer).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).
-export([default_layout/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/4, get_writee/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(SinkId, default_layout())
-spec new(logi_sink:id()) -> logi_sink:sink().
new(SinkId) ->
    new(SinkId, default_layout()).

%% @doc Creates a new sink
-spec new(logi_sink:id(), logi_layout:layout(unicode:chardata())) -> logi_sink:sink().
new(SinkId, Layout) ->
    _ = logi_layout:is_layout(Layout) orelse error(badarg, [Layout]),
    logi_sink:from_writer(SinkId, logi_sink_writer:new(?MODULE, Layout)).

%% @doc Default layout
%%
%% `Layout' is `logi_layout_newline:new(logi_layout_color:new(logi_layout_limit:new(logi_layout_default:new())))'
-spec default_layout() -> Layout :: logi_layout:layout(unicode:chardata()).
default_layout() ->
    logi_layout_newline:new(logi_layout_color:new(logi_layout_limit:new(logi_layout_default:new()))).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Format, Data, Layout) ->
    FormattedData = logi_layout:format(Context, Format, Data, Layout),
    _ = io:put_chars(user, FormattedData),
    FormattedData.

%% @private
get_writee(_) ->
    whereis(user).
