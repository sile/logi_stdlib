

# Module logi_layout_newline #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

TODO.

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_layout`](logi_layout.md).

<a name="types"></a>

## Data Types ##




### <a name="type-style">style()</a> ###


<pre><code>
style() = lf | cr | crlf
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(BaseLayout, lf)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new layout instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(BaseLayout::<a href="logi_layout.md#type-layout">logi_layout:layout()</a>) -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout()</a>
</code></pre>
<br />

Equivalent to [`new(BaseLayout, lf)`](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(BaseLayout::<a href="logi_layout.md#type-layout">logi_layout:layout()</a>, Style::<a href="#type-style">style()</a>) -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout()</a>
</code></pre>
<br />

Creates a new layout instance

