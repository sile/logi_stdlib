

# Module logi_sink_file_rotator_daily #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A logi_sink_file_rotator implementation which rotates files by day.

Copyright (c) 2015-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink_file_rotator`](logi_sink_file_rotator.md).

<a name="description"></a>

## Description ##


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > Rotator = logi_sink_file_rotator_daily:new().
  > Sink = logi_sink_file:new(foo, "/tmp/{YYYY}-{MM}-{DD}-sample.log", [{rotator, Rotator}]).
  > {ok, _} = logi_channel:install_sink(Sink, debug).
  > logi:info("hello world").
  > file:read_file("/tmp/2015-11-04-sample.log").
  {ok,<<"2015-11-04 00:47:39.105 [info] nonode@nohost <0.114.0> erl_eval:do_apply:673 [] hello world\n">>}
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Equivalent to <a href="#new-1"><tt>new(logi_sink_file_rotator_noop:new())</tt></a>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new rotator instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="logi_sink_file_rotator.md#type-rotator">logi_sink_file_rotator:rotator()</a>
</code></pre>
<br />

Equivalent to [`new(logi_sink_file_rotator_noop:new())`](#new-1).

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(BaseRotator::<a href="logi_sink_file_rotator.md#type-rotator">logi_sink_file_rotator:rotator()</a>) -&gt; <a href="logi_sink_file_rotator.md#type-rotator">logi_sink_file_rotator:rotator()</a>
</code></pre>
<br />

Creates a new rotator instance

