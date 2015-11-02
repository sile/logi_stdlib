

# Module logi_layout_color #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_layout`](logi_layout.md).

<a name="types"></a>

## Data Types ##




### <a name="type-color_fun">color_fun()</a> ###


<pre><code>
color_fun() = fun((<a href="logi_context.md#type-context">logi_context:context()</a>) -&gt; iodata())
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default_color-1">default_color/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(BaseLayout, fun logi_layout_color:default_color/1)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default_color-1"></a>

### default_color/1 ###

<pre><code>
default_color(Context::<a href="logi_context.md#type-context">logi_context:context()</a>) -&gt; iodata()
</code></pre>
<br />

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(BaseLayout::<a href="logi_layout.md#type-layout">logi_layout:layout()</a>) -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout()</a>
</code></pre>
<br />

Equivalent to [`new(BaseLayout, fun logi_layout_color:default_color/1)`](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(BaseLayout::<a href="logi_layout.md#type-layout">logi_layout:layout()</a>, Color::<a href="#type-color_fun">color_fun()</a>) -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout()</a>
</code></pre>
<br />

