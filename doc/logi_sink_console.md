

# Module logi_sink_console #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Console sink for logi.

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink`](logi_sink.md).

<a name="description"></a>

## Description ##

The sink outputs log messages to the user console.


### <a name="NOTE">NOTE</a> ###

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Creates a new sink instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink instance

The default layout of the sink is `logi_layout_color:new(logi_layout_limit:new(logi_layout_default:new()))`.

