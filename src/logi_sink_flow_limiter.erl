%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログの出力量を制御するためのシンク
%%
%% このシンクはログ出力の際に、以下の判定を行う:
%% - ログの書き込み先プロセスが生きているか
%% - ログの書き込み先プロセスのメッセージキューが詰まっていないか
%% - ログの出力ペースが指定の範囲内に収まっているか
%%
%% いずれかの条件を満たさなかった場合は、そのログメッセージは破棄される。
%% (破棄されたメッセージが存在する場合は、一定期間毎にまとめて、レポートが出力される)
%%
%% 全ての条件を満たしている場合は、実際のログ出力処理を担っているシンクに後続の処理が委譲される。
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > application:set_env(logi, warn_no_parse_transform, false). % Suppresses annoying warnings
%%
%% > {ok, _} = logi_sink_flow_limiter:start_limiter(sample_limiter, user, [{write_rate_limits, [{1024, 1000}]}]).
%% > Sink = logi_sink_flow_limiter:new(sample_limiter, logi_sink_console:new()).
%% > {ok, _} = logi_channel:install_sink(debug, Sink).
%%
%% > logi:notice("hello world").
%% 2015-11-04 08:59:29.269 [notice] nonode@nohost &lt;0.98.0> erl_eval:do_apply:673 [] hello world
%%
%% > lists:foreach(fun (I) -> logi:info("hello: ~p", [I]) end, lists:seq(1, 1000)).
%% 2015-11-04 08:59:53.364 [info] nonode@nohost &lt;0.98.0&gt; lists:foreach:1337 [] hello: 1
%% 2015-11-04 08:59:53.365 [info] nonode@nohost &lt;0.98.0&gt; lists:foreach:1337 [] hello: 2
%% 2015-11-04 08:59:53.366 [info] nonode@nohost &lt;0.98.0&gt; lists:foreach:1337 [] hello: 3
%% 2015-11-04 08:59:53.366 [info] nonode@nohost &lt;0.98.0&gt; lists:foreach:1337 [] hello: 4
%% 2015-11-04 08:59:53.366 [info] nonode@nohost &lt;0.98.0&gt; lists:foreach:1337 [] hello: 5
%% 2015-11-04 08:59:53.367 [info] nonode@nohost &lt;0.98.0&gt; lists:foreach:1337 [] hello: 6
%% 2015-11-04 08:59:53.367 [info] nonode@nohost &lt;0.98.0&gt; lists:foreach:1337 [] hello: 7
%% 2015-11-04 08:59:53.368 [info] nonode@nohost &lt;0.98.0&gt; lists:foreach:1337 [] hello: 8
%% 2015-11-04 08:59:53.368 [info] nonode@nohost &lt;0.98.0&gt; lists:foreach:1337 [] hello: 9
%% 2015-11-04 08:59:53.368 [info] nonode@nohost &lt;0.98.0&gt; lists:foreach:1337 [] hello: 10
%% 2015-11-04 08:59:53.369 [info] nonode@nohost &lt;0.98.0&gt; lists:foreach:1337 [] hello: 11
%% 2015-11-04 09:00:45.551 [warning] nonode@nohost &lt;0.113.0&gt; logi_sink_flow_limiter_server:report_omissions:203 [id=sample_limiter] Over a period of 60 seconds, 989 info messages were omitted: channel=logi_default_log, reason=rate_exceeded (e.g. [{pid,module,line},{&lt;0.98.0&gt;,lists,1337}])
%% </pre>
-module(logi_sink_flow_limiter).

-behaviour(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_limiter/2, start_limiter/3]).
-export([stop_limiter/1]).
-export([which_limiters/0]).

-export([new/2]).

-export_type([id/0]).
-export_type([options/0, option/0]).
-export_type([destination/0]).
-export_type([write_rate/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/5, default_layout/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type id() :: atom().
%% The identifier of a limiter

-type options() :: [option()].

-type option() :: {logger, logi:logger()}
                | {max_message_queue_len, non_neg_integer()}
                | {write_rate_limits, [write_rate()]}.
%% `logger':
%% - limiterが使用するロガー
%% - 破棄されたメッセージの情報等は、このロガーを使って報告される
%% - default: `logi:default_logger()'
%%
%% `max_message_queue_len':
%% - ログメッセージ書き込み時に許容される`destination()'プロセスの最大メッセージキュー長
%% - もしキューの長さがこの値を超えている場合は、該当メッセージは破棄される
%% - default: `256'
%%
%% `write_rate_limits':
%% - ログメッセージの書き込みレートの上限指定
%% - 全ての`write_rate()'を満たしている場合にのみ、新規ログメッセージが出力される
%% - 例: `[{10*1024*1024, 1000}, {500*1024*1024, 60*60*1000}]': 10MB/秒 and 500MB/時
%% - TODO: 詳細な挙動を書く (性能と正確さとのトレードオフ周りも)
%% - default: `[]'

-type destination() :: pid() | atom().
%% ログメッセージの実際の書き込み先プロセス (or その名前)
%%
%% このプロセスが死活判定やメッセージキュー詰まり判定の対象となる

-type write_rate() :: {Bytes::non_neg_integer(), Period::pos_integer()}. % TODO: Period::pos_milliseconds()
%% ログメッセージの書き込みレートの上限指定
%%
%% `Period'の期間中は、合計で`Bytes'までのメッセージ音書き込みが許容される

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv start_limiter(Id, Destination, [])
-spec start_limiter(id(), destination()) -> {ok, pid()} | {error, Reason::term()}.
start_limiter(Id, Destination) ->
    start_limiter(Id, Destination, []).

%% @doc Starts a new limiter
-spec start_limiter(id(), destination(), options()) -> {ok, pid()} | {error, Reason::term()}.
start_limiter(Id, Destination, Options) ->
    Args = [Id, Destination, Options],
    _ = is_atom(Id) orelse error(badarg, Args),
    _ = is_atom(Destination) orelse is_pid(Destination) orelse error(badarg, Args),
    _ = is_list(Options) orelse error(badarg, Args),

    Logger = proplists:get_value(logger, Options, logi:default_logger()),
    MaxLen = proplists:get_value(max_message_queue_len, Options, 256),
    WriteLimits = proplists:get_value(write_rate_limits, Options, []),
    _ = logi:is_logger(Logger) orelse error(badarg, Args),
    _ = (is_integer(MaxLen) andalso MaxLen >= 0) orelse error(badarg, Args),
    _ = (is_list(WriteLimits) andalso lists:all(fun is_write_rate/1, WriteLimits)) orelse error(badarg, Args),

    logi_sink_flow_limiter_server_sup:start_child(Id, {Destination, Logger, MaxLen, lists:usort(WriteLimits)}).

%% @doc Stops the limiter
%%
%% If the limiter associated to `Id' does not exists, it is silently ignored.
-spec stop_limiter(id()) -> ok.
stop_limiter(Id) ->
    logi_sink_flow_limiter_server_sup:stop_child(Id).

%% @doc Returns a list of the running limiters
-spec which_limiters() -> [id()].
which_limiters() ->
    [Id || {Id, _} <- logi_sink_flow_limiter_server_sup:which_children()].

%% @doc Creates a new sink instance
%%
%% The default layout of the sink is `logi_sink:default_layout(BaseSink)'.
-spec new(id(), logi_sink:sink()) -> logi_sink:sink().
new(Limiter, BaseSink) ->
    _ = is_atom(Limiter) orelse error(badarg, [Limiter, BaseSink]),
    _ = logi_sink:is_sink(BaseSink) orelse error(badarg, [Limiter, BaseSink]),
    logi_sink:new(?MODULE, {Limiter, BaseSink}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Layout, Format, Data, {Limiter, BaseSink}) ->
    case logi_sink_flow_limiter_server:get_destination_status(Limiter) of
        dead           -> logi_sink_flow_limiter_server:notify_omission(Limiter, destination_is_dead, Context);
        queue_overflow -> logi_sink_flow_limiter_server:notify_omission(Limiter, message_queue_overflow, Context);
        normal         ->
            case logi_sink_flow_limiter_server:is_rate_exceeded(Limiter) of
                true  -> logi_sink_flow_limiter_server:notify_omission(Limiter, rate_exceeded, Context);
                false ->
                    FormattedData = logi_layout:format(Context, Format, Data, Layout),
                    ok = logi_sink_flow_limiter_server:notify_write(Limiter, iolist_size(FormattedData)),
                    RawDataLayout = logi_layout:unsafe_new(logi_layout_raw, undefined),
                    (logi_sink:get_module(BaseSink)):write(Context, RawDataLayout, "", FormattedData, logi_sink:get_extra_data(BaseSink))
            end
    end.

%% @private
default_layout({_, BaseSink}) ->
    logi_sink:default_layout(BaseSink).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec is_write_rate(write_rate() | term()) -> boolean().
is_write_rate({Bytes, Period}) -> is_integer(Bytes) andalso Bytes >= 0 andalso is_integer(Period) andalso Period > 0;
is_write_rate(_)               -> false.
