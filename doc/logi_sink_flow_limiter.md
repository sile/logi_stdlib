

# Module logi_sink_flow_limiter #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink`](logi_sink.md).

<a name="types"></a>

## Data Types ##




### <a name="type-id">id()</a> ###


<pre><code>
id() = atom()
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Creates a new sink instance.</td></tr><tr><td valign="top"><a href="#start_limiter-1">start_limiter/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop_limiter-1">stop_limiter/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Limiter::<a href="#type-id">id()</a>, Destination::pid() | atom(), BaseSink::<a href="logi_sink.md#type-sink">logi_sink:sink()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink instance

The default layout of the sink is `logi_sink:default_layout(BaseSink)`.

<a name="start_limiter-1"></a>

### start_limiter/1 ###

<pre><code>
start_limiter(Id::<a href="#type-id">id()</a>) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

<a name="stop_limiter-1"></a>

### stop_limiter/1 ###

<pre><code>
stop_limiter(Id::<a href="#type-id">id()</a>) -&gt; ok
</code></pre>
<br />

