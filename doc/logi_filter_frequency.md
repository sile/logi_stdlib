

# Module logi_filter_frequency #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`logi_filter`](logi_filter.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#filter-2">filter/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Equivalent to <a href="#new-1"><tt>new([])</tt></a>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="filter-2"></a>

### filter/2 ###

`filter(Context, State0) -> any()`

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

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {intensity, non_neg_integer()} | {period, timeout()}</code></li></ul>

