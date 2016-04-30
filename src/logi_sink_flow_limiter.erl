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

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/2, new/3]).

-export_type([options/0, option/0]).
-export_type([agent_option/0]).
-export_type([write_rate/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type options() :: [option()].

-type option() :: agent_option().

-type agent_option() :: {logger, logi:logger()}
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

-type write_rate() :: {Bytes::non_neg_integer(), Period::pos_integer()}. % TODO: Period::pos_milliseconds()
%% ログメッセージの書き込みレートの上限指定
%%
%% `Period'の期間中は、合計で`Bytes'までのメッセージ音書き込みが許容される

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec new(logi_sink:id(), logi_sink:sink()) -> logi_sink:sink().
new(Id, BaseSink) ->
    new(Id, BaseSink, []).

-spec new(logi_sink:id(), logi_sink:sink(), options()) -> logi_sink:sink().
new(Id, BaseSink, Options) ->
    Args = [Id, BaseSink, Options],
    _ = logi_sink:is_sink(BaseSink) orelse error(badarg, Args),
    _ = is_list(Options) orelse error(badarg, Args),

    Logger = proplists:get_value(logger, Options, logi:default_logger()),
    MaxLen = proplists:get_value(max_message_queue_len, Options, 256),
    WriteLimits = proplists:get_value(write_rate_limits, Options, []),
    _ = logi:is_logger(Logger) orelse error(badarg, Args),
    _ = (is_integer(MaxLen) andalso MaxLen >= 0) orelse error(badarg, Args),
    _ = (is_list(WriteLimits) andalso lists:all(fun is_write_rate/1, WriteLimits)) orelse error(badarg, Args),

    StartArg = {Logger, MaxLen, lists:usort(WriteLimits), BaseSink},
    logi_sink:new(#{id => Id, start => {logi_sink_flow_limiter_writer, start_link, [StartArg]}}).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec is_write_rate(write_rate() | term()) -> boolean().
is_write_rate({Bytes, Period}) -> is_integer(Bytes) andalso Bytes >= 0 andalso is_integer(Period) andalso Period > 0;
is_write_rate(_)               -> false.
