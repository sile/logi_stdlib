

# Module logi_layout_default #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

A logi_layout implementation which is used as the default layout in this application.

Copyright (c) 2015-2016 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_layout`](logi_layout.md).

<a name="description"></a>

## Description ##
This module layouts a log message by the following format:

```
  {yyyy}-{MM}-{dd} {HH}:{mm}:{ss}.{SSS} [{SEVERITY}] {NODE} {PID} {MODULE}:{FUNCTION}:{LINE} [{HEADER(KEY=VALUE)}*] {MESSAGE}
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Creates a new layout instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="logi_layout.md#type-layout">logi_layout:layout</a>(binary())
</code></pre>
<br />

Creates a new layout instance

