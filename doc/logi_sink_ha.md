

# Module logi_sink_ha #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A sink which provides HA (High Availability) functionality.

Copyright (c) 2015-2016 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-option">option()</a> ###


<pre><code>
option() = {logger, <a href="logi.md#type-logger">logi:logger()</a>} | {strategy, <a href="#type-select_strategy">select_strategy()</a>}
</code></pre>

`logger`:
- The logger instance which is used to report internal events of the sink process
- Default: `logi:default_logger()`

`strategy`:
- Peer selection strategy
- Default: `random`



### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>




### <a name="type-peer">peer()</a> ###


<pre><code>
peer() = #{sink =&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>, restart =&gt; <a href="#type-restart_strategy">restart_strategy()</a>}
</code></pre>

 A peer specification

`restart` is optional field (default value is `#{interval => {1000, 60000}}`).



### <a name="type-restart_strategy">restart_strategy()</a> ###


<pre><code>
restart_strategy() = #{interval =&gt; timeout() | {Min::timeout(), Max::timeout()}}
</code></pre>

 Exited peer restarting strategy



### <a name="type-select_strategy">select_strategy()</a> ###


<pre><code>
select_strategy() = first_available | random
</code></pre>

 Peer selection strategy:
- `first_available`: Selects first available peer in the list
- `random`: Selects random peer in the list

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Equivalent to <a href="#new-3"><tt>new(SinkId, Peers, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Peers::[<a href="#type-peer">peer()</a>]) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Equivalent to [`new(SinkId, Peers, [])`](#new-3).

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Peers::[<a href="#type-peer">peer()</a>], Options::<a href="#type-options">options()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

