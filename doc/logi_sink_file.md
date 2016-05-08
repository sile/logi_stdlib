

# Module logi_sink_file #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-filepath">filepath()</a> ###


<pre><code>
filepath() = <a href="file.md#type-name_all">file:name_all()</a>
</code></pre>

 A log file path



### <a name="type-open_options">open_options()</a> ###


<pre><code>
open_options() = list()
</code></pre>



### <a name="type-option">option()</a> ###


<pre><code>
option() = {layout, <a href="logi_layout.md#type-layout">logi_layout:layout()</a>} | {logger, <a href="logi.md#type-logger">logi:logger()</a>} | {rotator, <a href="logi_sink_file_rotator.md#type-rotator">logi_sink_file_rotator:rotator()</a>} | {open_opt, <a href="#type-open_options">open_options()</a>}
</code></pre>



### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default_layout-0">default_layout/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default_layout-0"></a>

### default_layout/0 ###

<pre><code>
default_layout() -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout()</a>
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::<a href="logi_sink.md#type-id">logi_sink:id()</a>, FilePath::<a href="#type-filepath">filepath()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Id::<a href="logi_sink.md#type-id">logi_sink:id()</a>, FilePath::<a href="#type-filepath">filepath()</a>, Options::<a href="#type-options">options()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

