

# Module logi_filter_compose #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

A logi_filter implementation filter which is composed of sub-filters combined by logical operators.

Copyright (c) 2015-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_filter`](logi_filter.md).

<a name="types"></a>

## Data Types ##




### <a name="type-expression">expression()</a> ###


<pre><code>
expression() = {'not', <a href="#type-expression">expression()</a>} | {'and', [<a href="#type-expression">expression()</a>]} | {'or', [<a href="#type-expression">expression()</a>]} | <a href="logi_filter.md#type-filter">logi_filter:filter()</a>
</code></pre>

 Logical operation expression which represents a composite filter.

Expressions are evaluated in the short-circuit manner.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new filter instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Expression::<a href="#type-expression">expression()</a>) -&gt; <a href="logi_filter.md#type-filter">logi_filter:filter()</a>
</code></pre>
<br />

Creates a new filter instance

