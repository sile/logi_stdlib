%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A sink which limits message flow rate of underlying sink
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > error_logger:tty(false). % Suppresses annoying warnings for the sake of brevity
%%
%% > Sink = logi_sink_flow_limiter:new(limiter, logi_sink_console:new(console), [{write_rate_limits, [{1024, 1000}]}]).
%% > {ok, _} = logi_channel:install_sink(Sink, debug).
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
%% @end
%%
%% TODO: 以下を英訳して、Edocドキュメントに含める
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
-module(logi_sink_flow_limiter).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/2, new/3]).

-export_type([options/0, option/0]).
-export_type([write_rate/0]).
-export_type([pos_milliseconds/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type options() :: [option()].

-type option() :: {logger, logi:logger()}
                | {max_message_queue_len, non_neg_integer()}
                | {write_rate_limits, [write_rate()]}.
%% `logger':
%% - The logger instance which is used to report internal events (e.g., message discarding) of the sink process
%% - Default: `logi:default_logger()'
%%
%% `max_message_queue_len':
%% - Maximum message queue length of the writee process of the underlying sink
%% - While the queue length exceeds the value, messages will be discarded
%% - default: `256'
%%
%% `write_rate_limits':
%% - Log messages write rate limit specification
%% - If all `write_rate()' are satisfied, new arrival message will be outputed
%% - e.g., `[{10*1024*1024, 1000}, {500*1024*1024, 60*60*1000}]': 10MB/seconds and 500MB/seconds
%% - default: `[]'

-type write_rate() :: {Bytes::non_neg_integer(), Period::pos_milliseconds()}.
%% Write rate limit specification
%%
%% In `Period' milliseconds, it is allowed to write messages of up to `Bytes' bytes.

-type pos_milliseconds() :: pos_integer().
%% Positive milliseconds

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(Sink, BaseSink)
-spec new(logi_sink:id(), logi_sink:sink()) -> logi_sink:sink().
new(SinkId, BaseSink) ->
    new(SinkId, BaseSink, []).

%% @doc Creates a new sink
-spec new(logi_sink:id(), logi_sink:sink(), options()) -> logi_sink:sink().
new(SinkId, BaseSink, Options) ->
    Args = [SinkId, BaseSink, Options],
    _ = logi_sink:is_sink(BaseSink) orelse error(badarg, Args),
    _ = is_list(Options) orelse error(badarg, Args),

    Logger = proplists:get_value(logger, Options, logi:default_logger()),
    MaxLen = proplists:get_value(max_message_queue_len, Options, 256),
    WriteLimits = proplists:get_value(write_rate_limits, Options, []),
    _ = logi:is_logger(Logger) orelse error(badarg, Args),
    _ = (is_integer(MaxLen) andalso MaxLen >= 0) orelse error(badarg, Args),
    _ = (is_list(WriteLimits) andalso lists:all(fun is_write_rate/1, WriteLimits)) orelse error(badarg, Args),

    StartArg = {Logger, MaxLen, lists:usort(WriteLimits), BaseSink},
    logi_sink:new(#{id => SinkId, start => {logi_sink_flow_limiter_writer, start_link, [StartArg]}}).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec is_write_rate(write_rate() | term()) -> boolean().
is_write_rate({Bytes, Period}) -> is_integer(Bytes) andalso Bytes >= 0 andalso is_integer(Period) andalso Period > 0;
is_write_rate(_)               -> false.
