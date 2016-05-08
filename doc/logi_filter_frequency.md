

# Module logi_filter_frequency #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A logi_filter implementation to control log message output frequency.

Copyright (c) 2015-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_filter`](logi_filter.md).

<a name="types"></a>

## Data Types ##




### <a name="type-pos_milliseconds">pos_milliseconds()</a> ###


<pre><code>
pos_milliseconds() = pos_integer()
</code></pre>

 Positive milli-seconds

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Equivalent to <a href="#new-1"><tt>new([])</tt></a>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new filter instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="logi_filter.md#type-filter">logi_filter:filter()</a>
</code></pre>
<br />

Equivalent to [`new([])`](#new-1).

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Options) -&gt; <a href="logi_filter.md#type-filter">logi_filter:filter()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {max_count, pos_integer()} | {period, <a href="#type-pos_milliseconds">pos_milliseconds()</a>}</code></li></ul>

Creates a new filter instance


#### <a name="OPTIONS">OPTIONS</a> ####

`max_count`:
- Maximum log message count allowed in the given period
- Default: `5`

`period`:
- Frequency control period
- Default: `60000`

