

# Module logi_sink_file #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

TODO.

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink`](logi_sink.md).

<a name="types"></a>

## Data Types ##




### <a name="type-writer_id">writer_id()</a> ###


<pre><code>
writer_id() = atom()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new sink instance.</td></tr><tr><td valign="top"><a href="#start_writer-2">start_writer/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop_writer-1">stop_writer/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Writer::<a href="#type-writer_id">writer_id()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink instance

The default layout of the sink is `logi_layout_limit:new(logi_layout_default:new())`.

<a name="start_writer-2"></a>

### start_writer/2 ###

<pre><code>
start_writer(WriterId::<a href="#type-writer_id">writer_id()</a>, PathGen::<a href="logi_sink_file_path.md#type-path">logi_sink_file_path:path()</a>) -&gt; {ok, MayChange::pid()} | {error, Reason::term()}
</code></pre>
<br />

<a name="stop_writer-1"></a>

### stop_writer/1 ###

<pre><code>
stop_writer(WriterId::<a href="#type-writer_id">writer_id()</a>) -&gt; ok
</code></pre>
<br />

