

# Module logi_sink_ha #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-option">option()</a> ###


<pre><code>
option() = {logger, <a href="logi.md#type-logger">logi:logger()</a>} | {strategy, <a href="#type-select_strategy">select_strategy()</a>}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>




### <a name="type-peer">peer()</a> ###


<pre><code>
peer() = #{sink =&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>, restart =&gt; <a href="#type-restart_strategy">restart_strategy()</a>}
</code></pre>




### <a name="type-restart_strategy">restart_strategy()</a> ###


<pre><code>
restart_strategy() = #{interval =&gt; non_neg_integer() | {non_neg_integer(), non_neg_integer()}}
</code></pre>




### <a name="type-select_strategy">select_strategy()</a> ###


<pre><code>
select_strategy() = first_available | random
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Id::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Peers::[<a href="#type-peer">peer()</a>]) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Id::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Peers::[<a href="#type-peer">peer()</a>], Options::<a href="#type-options">options()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

