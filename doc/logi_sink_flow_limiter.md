

# Module logi_sink_flow_limiter #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-agent_option">agent_option()</a> ###


<pre><code>
agent_option() = {logger, <a href="logi.md#type-logger">logi:logger()</a>} | {max_message_queue_len, non_neg_integer()} | {write_rate_limits, [<a href="#type-write_rate">write_rate()</a>]}
</code></pre>



### <a name="type-option">option()</a> ###


<pre><code>
option() = <a href="#type-agent_option">agent_option()</a>
</code></pre>




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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::<a href="logi_sink.md#type-id">logi_sink:id()</a>, BaseSink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Id::<a href="logi_sink.md#type-id">logi_sink:id()</a>, BaseSink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>, Options::<a href="#type-options">options()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

