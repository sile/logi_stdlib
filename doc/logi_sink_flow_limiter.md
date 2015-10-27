

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#install-1">install/1</a></td><td>Equivalent to <a href="#install-2"><tt>install(Condition, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#install-2">install/2</a></td><td>Installs a sink.</td></tr><tr><td valign="top"><a href="#start_limiter-1">start_limiter/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop_limiter-1">stop_limiter/1</a></td><td></td></tr><tr><td valign="top"><a href="#uninstall-0">uninstall/0</a></td><td>Equivalent to <a href="#uninstall-1"><tt>uninstall([])</tt></a>.</td></tr><tr><td valign="top"><a href="#uninstall-1">uninstall/1</a></td><td>Uninstalls a sink.</td></tr><tr><td valign="top"><a href="#write-4">write/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="install-1"></a>

### install/1 ###

<pre><code>
install(Condition::<a href="logi_sink.md#type-condition">logi_sink:condition()</a>) -&gt; <a href="logi_channel.md#type-install_sink_result">logi_channel:install_sink_result()</a>
</code></pre>
<br />

Equivalent to [`install(Condition, [])`](#install-2).

<a name="install-2"></a>

### install/2 ###

<pre><code>
install(Condition::<a href="logi_sink.md#type-condition">logi_sink:condition()</a>, Options) -&gt; <a href="logi_channel.md#type-install_sink_result">logi_channel:install_sink_result()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {id, <a href="logi_sink.md#type-id">logi_sink:id()</a>} | {channel, <a href="logi_channel.md#type-id">logi_channel:id()</a>} | {layout, <a href="logi_layout.md#type-layout">logi_layout:layout()</a>} | {sink, <a href="logi_sink.md#type-sink">logi_sink:sink()</a>} | {limiter, <a href="#type-id">id()</a>} | <a href="logi_channel.md#type-install_sink_option">logi_channel:install_sink_option()</a></code></li></ul>

Installs a sink

The default value of `Options`:
- id: `logi_sink_flow_limiter`
- channel: `logi_channel:default_channel()`
- layout: `logi_layout_color:new(logi_builtin_layout_simple:new())` TODO: logi_layout_default

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

<a name="uninstall-0"></a>

### uninstall/0 ###

<pre><code>
uninstall() -&gt; <a href="logi_channel.md#type-uninstall_sink_result">logi_channel:uninstall_sink_result()</a>
</code></pre>
<br />

Equivalent to [`uninstall([])`](#uninstall-1).

<a name="uninstall-1"></a>

### uninstall/1 ###

<pre><code>
uninstall(Options) -&gt; <a href="logi_channel.md#type-uninstall_sink_result">logi_channel:uninstall_sink_result()</a>
</code></pre>

<ul class="definitions"><li><code>Options = [Option]</code></li><li><code>Option = {id, <a href="logi_sink.md#type-id">logi_sink:id()</a>} | {channel, <a href="logi_channel.md#type-id">logi_channel:id()</a>}</code></li></ul>

Uninstalls a sink

The default value of `Options`:
- id: `logi_sink_flow_limiter`
- channel: `logi_channel:default_channel()`

<a name="write-4"></a>

### write/4 ###

`write(Context, Format, Data, State) -> any()`

