

# Module logi_sink_file #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A sink which writes log messages to a file.

Copyright (c) 2015-2016 Takeru Ohta <phjgt308@gmail.com>

<a name="description"></a>

## Description ##


### <a name="NOTE">NOTE</a> ###

The sink has no overload protections,
so it is recommended to use it together with (for example) [`logi_slink_flow_limiter`](logi_slink_flow_limiter.md)
in a production environment.


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > {ok, _} =  logi_sink_file:start_writer(sample_file_writer, <<"/tmp/sample.log">>).
  > {ok, _} = logi_channel:install_sink(debug, logi_sink_file:new(sample_file_writer)).
  > logi:info("hello world").
  > file:read_file("/tmp/sample.log").
  {ok,<<"2015-11-04 00:13:33.058 [info] nonode@nohost <0.98.0> erl_eval:do_apply:673 [] hello world\n">>}
```

<a name="types"></a>

## Data Types ##




### <a name="type-filepath">filepath()</a> ###


<pre><code>
filepath() = binary()
</code></pre>

 A log file path



### <a name="type-open_options">open_options()</a> ###


<pre><code>
open_options() = list()
</code></pre>

 Log file open options

See [file:mode/0](http://www.erlang.org/doc/man/file.html#type-mode) for more details



### <a name="type-option">option()</a> ###


<pre><code>
option() = {layout, <a href="logi_layout.md#type-layout">logi_layout:layout()</a>} | {logger, <a href="logi.md#type-logger">logi:logger()</a>} | {rotator, <a href="logi_sink_file_rotator.md#type-rotator">logi_sink_file_rotator:rotator()</a>} | {open_opt, <a href="#type-open_options">open_options()</a>}
</code></pre>

`layout`:
- The layout instance used by the sink
- Default: `logi_sink_file:default_layout()`

`logger`:
- The logger instance which is used to report internal events of the sink process
- Default: `logi:default_logger()`

`rotator`:
- The rotator instance used by the sink
- Default: `logi_sink_file_rotator_noop:new()`

`open_opt`:
- Log file open options (i.e., the second argument of `file:open/2`)
- Default: `[append, raw, delayed_write]`



### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default_layout-0">default_layout/0</a></td><td>Default layout.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Equivalent to <a href="#new-3"><tt>new(SinkId, FilePath, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Creates a new sink.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default_layout-0"></a>

### default_layout/0 ###

<pre><code>
default_layout() -&gt; Layout::<a href="logi_layout.md#type-layout">logi_layout:layout()</a>
</code></pre>
<br />

Default layout

`Layout` is `logi_layout_newline:new(logi_layout_limit:new(logi_layout_default:new()))`.

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, FilePath::<a href="file.md#type-name_all">file:name_all()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Equivalent to [`new(SinkId, FilePath, [])`](#new-3).

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, FilePath::<a href="file.md#type-name_all">file:name_all()</a>, Options::<a href="#type-options">options()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink

