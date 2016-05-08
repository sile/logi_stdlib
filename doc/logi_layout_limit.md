

# Module logi_layout_limit #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A logi_layout implementation which limits the size of a log message.

Copyright (c) 2015-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_layout`](logi_layout.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(BaseLayout, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new layout instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(BaseLayout::<a href="logi_layout.md#type-layout">logi_layout:layout()</a>) -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout()</a>
</code></pre>
<br />

Equivalent to [`new(BaseLayout, [])`](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(BaseLayout::<a href="logi_layout.md#type-layout">logi_layout:layout()</a>, Options) -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {max_width, pos_integer() | infinity} | {max_depth, pos_integer() | infinity} | {max_size, pos_integer() | infinity}</code></li></ul>

Creates a new layout instance

