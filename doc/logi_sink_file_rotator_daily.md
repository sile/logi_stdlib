

# Module logi_sink_file_rotator_daily #
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink_file_rotator`](logi_sink_file_rotator.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Equivalent to <a href="#new-1"><tt>new(logi_sink_file_rotator_do_nothing:new())</tt></a>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new rotator instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="logi_sink_file_rotator.md#type-rotator">logi_sink_file_rotator:rotator()</a>
</code></pre>
<br />

Equivalent to [`new(logi_sink_file_rotator_do_nothing:new())`](#new-1).

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(BaseRotator::<a href="logi_sink_file_rotator.md#type-rotator">logi_sink_file_rotator:rotator()</a>) -&gt; <a href="logi_sink_file_rotator.md#type-rotator">logi_sink_file_rotator:rotator()</a>
</code></pre>
<br />

Creates a new rotator instance

