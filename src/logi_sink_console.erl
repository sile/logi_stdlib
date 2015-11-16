%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc コンソール出力用のシンク
%%
%% == NOTE ==
%% このシンク自体には過負荷防止の仕組みはないので本番環境で使用する場合は{@link logi_sink_flow_limiter}等との併用が推奨される。
%% また調査時に一時的にログを出力したいだけなら`logi_channel:install_sink_option/0'の`lifetime'オプションの指定を検討しても良い。
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > logi_channel:install_sink(info, logi_sink_console:new()).
%% > logi:info("hello world").
%% 2015-11-03 10:58:59.920 [info] nonode@nohost &lt;0.113.0&gt; erl_eval:do_apply:673 [] hello world
%% </pre>
%%
%% 別のレイアウトで出力する:
%% <pre lang="erlang">
%% > Layout = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format(Format, Data) end),
%% > logi_channel:install_sink(info, logi_sink_console:new(), [{layout, Layout}, {if_exists, supersede}]).
%% > logi:info("hello world").
%% hello world
%% </pre>
-module(logi_sink_console).

-behaviour(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0, new/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/3]).
-export([whereis_agent/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(logi_layout_newline:new(logi_layout_color:new(logi_layout_limit:new(logi_layout_default:new()))))
new() ->
    new(logi_layout_newline:new(logi_layout_color:new(logi_layout_limit:new(logi_layout_default:new())))).

%% @doc Creates a new sink instance
-spec new(logi_layout:layout(unicode:chardata())) -> logi_sink:spec().
new(Layout) ->
    _ = logi_layout:is_layout(Layout) orelse error(badarg, [Layout]),
    AgentSpec = logi_agent:new_external(user, logi_restart_strategy_backoff:new(), undefined),
    logi_sink:new(?MODULE, Layout, AgentSpec).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(_Context, FormattedData, _) ->
    io:put_chars(user, FormattedData).

%% @private
whereis_agent(_) ->
    whereis(user).
