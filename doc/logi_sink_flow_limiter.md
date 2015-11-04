

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

TODO
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



### <a name="type-options">options()</a> ###


<pre><code>
options() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Creates a new sink instance.</td></tr><tr><td valign="top"><a href="#start_limiter-1">start_limiter/1</a></td><td>Equivalent to <a href="#start_limiter-2"><tt>start_limiter(Id, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#start_limiter-2">start_limiter/2</a></td><td>Starts a new limiter.</td></tr><tr><td valign="top"><a href="#stop_limiter-1">stop_limiter/1</a></td><td>Stops the limiter.</td></tr><tr><td valign="top"><a href="#which_limiters-0">which_limiters/0</a></td><td>Returns a list of the running limiters.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Limiter::<a href="#type-id">id()</a>, Destination::<a href="#type-destination">destination()</a>, BaseSink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink instance

The default layout of the sink is `logi_sink:default_layout(BaseSink)`.

ログメッセージ出力の際に`Destination`が、`Limiter`起動時に指定された条件を満たしている場合は、
`BaseSink`を用いて実際のログの出力処理が行われる。

<a name="start_limiter-1"></a>

### start_limiter/1 ###

<pre><code>
start_limiter(Id::<a href="#type-id">id()</a>) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

Equivalent to [`start_limiter(Id, [])`](#start_limiter-2).

<a name="start_limiter-2"></a>

### start_limiter/2 ###

<pre><code>
start_limiter(Id::<a href="#type-id">id()</a>, Options::<a href="#type-options">options()</a>) -&gt; {ok, pid()} | {error, Reason::term()}
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

