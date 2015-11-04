

# Module logi_sink_flow_limiter #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

ログの出力量を制御するためのシンク.

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink`](logi_sink.md).

<a name="description"></a>

## Description ##

このシンクはログ出力の際に、以下の判定を行う:
- ログの書き込み先プロセスが生きているか
- ログの書き込み先プロセスのメッセージキューが詰まっていないか
- ログの出力ペースが指定の範囲内に収まっているか

いずれかの条件を満たさなかった場合は、そのログメッセージは破棄される。
(破棄されたメッセージが存在する場合は、一定期間毎にまとめて、レポートが出力される)

全ての条件を満たしている場合は、実際のログ出力処理を担っているシンクに後続の処理が委譲される。


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > application:set_env(logi, warn_no_parse_transform, false). % Suppresses annoying warnings
  > {ok, _} = logi_sink_flow_limiter:start_limiter(sample_limiter, user, [{write_rate_limits, [{1024, 1000}]}]).
  > Sink = logi_sink_flow_limiter:new(sample_limiter, logi_sink_console:new()).
  > {ok, _} = logi_channel:install_sink(debug, Sink).
  > logi:notice("hello world").
  2015-11-04 08:59:29.269 [notice] nonode@nohost <0.98.0> erl_eval:do_apply:673 [] hello world
  > lists:foreach(fun (I) -> logi:info("hello: ~p", [I]) end, lists:seq(1, 1000)).
  2015-11-04 08:59:53.364 [info] nonode@nohost <0.98.0> lists:foreach:1337 [] hello: 1
  2015-11-04 08:59:53.365 [info] nonode@nohost <0.98.0> lists:foreach:1337 [] hello: 2
  2015-11-04 08:59:53.366 [info] nonode@nohost <0.98.0> lists:foreach:1337 [] hello: 3
  2015-11-04 08:59:53.366 [info] nonode@nohost <0.98.0> lists:foreach:1337 [] hello: 4
  2015-11-04 08:59:53.366 [info] nonode@nohost <0.98.0> lists:foreach:1337 [] hello: 5
  2015-11-04 08:59:53.367 [info] nonode@nohost <0.98.0> lists:foreach:1337 [] hello: 6
  2015-11-04 08:59:53.367 [info] nonode@nohost <0.98.0> lists:foreach:1337 [] hello: 7
  2015-11-04 08:59:53.368 [info] nonode@nohost <0.98.0> lists:foreach:1337 [] hello: 8
  2015-11-04 08:59:53.368 [info] nonode@nohost <0.98.0> lists:foreach:1337 [] hello: 9
  2015-11-04 08:59:53.368 [info] nonode@nohost <0.98.0> lists:foreach:1337 [] hello: 10
  2015-11-04 08:59:53.369 [info] nonode@nohost <0.98.0> lists:foreach:1337 [] hello: 11
  2015-11-04 09:00:45.551 [warning] nonode@nohost <0.113.0> logi_sink_flow_limiter_server:report_omissions:203 [id=sample_limiter] Over a period of 60 seconds, 989 info messages were omitted: channel=logi_default_log, reason=rate_exceeded (e.g. [{pid,module,line},{<0.98.0>,lists,1337}])
```

<a name="types"></a>

## Data Types ##




### <a name="type-destination">destination()</a> ###


<pre><code>
destination() = pid() | atom()
</code></pre>

 ログメッセージの実際の書き込み先プロセス (or その名前)

このプロセスが死活判定やメッセージキュー詰まり判定の対象となる



### <a name="type-id">id()</a> ###


<pre><code>
id() = atom()
</code></pre>

 The identifier of a limiter



### <a name="type-option">option()</a> ###


<pre><code>
option() = {logger, <a href="logi.md#type-logger">logi:logger()</a>} | {max_message_queue_len, non_neg_integer()} | {write_rate_limits, [<a href="#type-write_rate">write_rate()</a>]}
</code></pre>

`logger`:
- limiterが使用するロガー
- 破棄されたメッセージの情報等は、このロガーを使って報告される
- default: `logi:default_logger()`

`max_message_queue_len`:
- ログメッセージ書き込み時に許容される`destination()`プロセスの最大メッセージキュー長
- もしキューの長さがこの値を超えている場合は、該当メッセージは破棄される
- default: `256`

`write_rate_limits`:
- ログメッセージの書き込みレートの上限指定
- 全ての`write_rate()`を満たしている場合にのみ、新規ログメッセージが出力される
- 例: `[{10*1024*1024, 1000}, {500*1024*1024, 60*60*1000}]`: 10MB/秒 and 500MB/時
- TODO: 詳細な挙動を書く (性能と正確さとのトレードオフ周りも)
- default: `[]`



### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>




### <a name="type-write_rate">write_rate()</a> ###


<pre><code>
write_rate() = {Bytes::non_neg_integer(), Period::pos_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new sink instance.</td></tr><tr><td valign="top"><a href="#start_limiter-2">start_limiter/2</a></td><td>Equivalent to <a href="#start_limiter-3"><tt>start_limiter(Id, Destination, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#start_limiter-3">start_limiter/3</a></td><td>Starts a new limiter.</td></tr><tr><td valign="top"><a href="#stop_limiter-1">stop_limiter/1</a></td><td>Stops the limiter.</td></tr><tr><td valign="top"><a href="#which_limiters-0">which_limiters/0</a></td><td>Returns a list of the running limiters.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Limiter::<a href="#type-id">id()</a>, BaseSink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink instance

The default layout of the sink is `logi_sink:default_layout(BaseSink)`.

<a name="start_limiter-2"></a>

### start_limiter/2 ###

<pre><code>
start_limiter(Id::<a href="#type-id">id()</a>, Destination::<a href="#type-destination">destination()</a>) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

Equivalent to [`start_limiter(Id, Destination, [])`](#start_limiter-3).

<a name="start_limiter-3"></a>

### start_limiter/3 ###

<pre><code>
start_limiter(Id::<a href="#type-id">id()</a>, Destination::<a href="#type-destination">destination()</a>, Options::<a href="#type-options">options()</a>) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

Starts a new limiter

<a name="stop_limiter-1"></a>

### stop_limiter/1 ###

<pre><code>
stop_limiter(Id::<a href="#type-id">id()</a>) -&gt; ok
</code></pre>
<br />

Stops the limiter

If the limiter associated to `Id` does not exists, it is silently ignored.

<a name="which_limiters-0"></a>

### which_limiters/0 ###

<pre><code>
which_limiters() -&gt; [<a href="#type-id">id()</a>]
</code></pre>
<br />

Returns a list of the running limiters

