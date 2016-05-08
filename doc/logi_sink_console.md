

# Module logi_sink_console #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A sink which prints log messages to the console.

Copyright (c) 2015-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink_writer`](logi_sink_writer.md).

<a name="description"></a>

## Description ##


### <a name="NOTE">NOTE</a> ###

The sink has no overload protections,
so it is recommended to use it together with (for example) [`logi_slink_flow_limiter`](logi_slink_flow_limiter.md)
in a production environment.


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > error_logger:tty(false). % Suppresses annoying warnings for the sake of brevity
  >
  > logi_channel:install_sink(logi_sink_console:new(foo), info).
  > logi:info("hello world").
  2015-11-03 10:58:59.920 [info] nonode@nohost <0.113.0> erl_eval:do_apply:673 [] hello world
```

Uses other layout:

```erlang

  > logi_channel:install_sink(logi_sink_console:new(foo, logi_layout_io_lib_format:new()), info).
  > logi:info("hello world").
  hello world
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default_layout-0">default_layout/0</a></td><td>Default layout.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(SinkId, default_layout())</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new sink.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default_layout-0"></a>

### default_layout/0 ###

<pre><code>
default_layout() -&gt; Layout::<a href="logi_layout.md#type-layout">logi_layout:layout</a>(<a href="unicode.md#type-chardata">unicode:chardata()</a>)
</code></pre>
<br />

Default layout

`Layout` is `logi_layout_newline:new(logi_layout_color:new(logi_layout_limit:new(logi_layout_default:new())))`

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Equivalent to [`new(SinkId, default_layout())`](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Layout::<a href="logi_layout.md#type-layout">logi_layout:layout</a>(<a href="unicode.md#type-chardata">unicode:chardata()</a>)) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink

