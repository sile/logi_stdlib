

# Module logi_filter_severity #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A logi_filter implementation which filters log messages by given severity condition.

Copyright (c) 2015-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_filter`](logi_filter.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new filter instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(SeverityCondition::<a href="logi_condition.md#type-severity_condition">logi_condition:severity_condition()</a>) -&gt; <a href="logi_filter.md#type-filter">logi_filter:filter()</a>
</code></pre>
<br />

Creates a new filter instance

If a log message does not match `SeverityCondition`, it is discarded by the filter.

