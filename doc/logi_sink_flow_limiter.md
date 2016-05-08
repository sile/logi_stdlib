

# Module logi_sink_flow_limiter #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A sink which limits message flow rate of underlying sink.

Copyright (c) 2015-2016 Takeru Ohta <phjgt308@gmail.com>

<a name="description"></a>

## Description ##


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > error_logger:tty(false). % Suppresses annoying warnings for the sake of brevity
  > Sink = logi_sink_flow_limiter:new(limiter, logi_sink_console:new(console), [{write_rate_limits, [{1024, 1000}]}]).
  > {ok, _} = logi_channel:install_sink(Sink, debug).
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




### <a name="type-option">option()</a> ###


<pre><code>
option() = {logger, <a href="logi.md#type-logger">logi:logger()</a>} | {max_message_queue_len, non_neg_integer()} | {write_rate_limits, [<a href="#type-write_rate">write_rate()</a>]}
</code></pre>

`logger`:
- The logger instance which is used to report internal events (e.g., message discarding) of the sink process
- Default: `logi:default_logger()`

`max_message_queue_len`:
- Maximum message queue length of the writee process of the underlying sink
- While the queue length exceeds the value, messages will be discarded
- default: `256`

`write_rate_limits`:
- Log messages write rate limit specification
- If all `write_rate()` are satisfied, new arrival message will be outputed
- e.g., `[{10*1024*1024, 1000}, {500*1024*1024, 60*60*1000}]`: 10MB/seconds and 500MB/seconds
- default: `[]`



### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>




### <a name="type-pos_milliseconds">pos_milliseconds()</a> ###


<pre><code>
pos_milliseconds() = pos_integer()
</code></pre>

 Positive milliseconds



### <a name="type-write_rate">write_rate()</a> ###


<pre><code>
write_rate() = {Bytes::non_neg_integer(), Period::<a href="#type-pos_milliseconds">pos_milliseconds()</a>}
</code></pre>

 Write rate limit specification

In `Period` milliseconds, it is allowed to write messages of up to `Bytes` bytes.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Equivalent to <a href="#new-2"><tt>new(Sink, BaseSink)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Creates a new sink.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, BaseSink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Equivalent to [`new(Sink, BaseSink)`](#new-2).

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, BaseSink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>, Options::<a href="#type-options">options()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink

