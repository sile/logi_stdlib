

# Module logi_sink_ha #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

高可用性(HA, High Availability)を達成するための手段を提供するシンク.

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink`](logi_sink.md).

<a name="description"></a>

## Description ##

機能:
- Active/StandBy構成による多重化
- Active/Active構成による多重化
- 異常終了したプロセスのバックオフ付きの自動再起動

memo:
- 監視ツリーは別に存在するけど、そっちのsupervisorが諦めた場合に手助けをするスタンス
- HA起動時の自動起動や、HA停止時の自動停止は行わない


### <a name="EXAMPLE">EXAMPLE</a> ###

TODO
<a name="types"></a>

## Data Types ##




### <a name="type-destination">destination()</a> ###


<pre><code>
destination() = #{id =&gt; atom(), start =&gt; <a href="#type-mfargs">mfargs()</a>, restart =&gt; <a href="#type-restart_strategy">restart_strategy()</a>, sink =&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>, monitor_id =&gt; atom()}
</code></pre>




### <a name="type-manager_id">manager_id()</a> ###


<pre><code>
manager_id() = atom()
</code></pre>

 The identifier of a HA manager



### <a name="type-mfargs">mfargs()</a> ###


<pre><code>
mfargs() = {module(), atom(), [term()]}
</code></pre>




### <a name="type-mode">mode()</a> ###


<pre><code>
mode() = active_passive | active_active
</code></pre>




### <a name="type-option">option()</a> ###


<pre><code>
option() = {logger, <a href="logi.md#type-logger">logi:logger()</a>} | {mode, <a href="#type-mode">mode()</a>}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>




### <a name="type-restart_strategy">restart_strategy()</a> ###


<pre><code>
restart_strategy() = {exponential_backoff, Min::non_neg_integer(), Max::non_neg_integer()} | {constant, Time::non_neg_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new sink instance.</td></tr><tr><td valign="top"><a href="#start_manager-2">start_manager/2</a></td><td>Equivalent to <a href="#start_manager-3"><tt>start_manager(ManagerId, Destinations, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#start_manager-3">start_manager/3</a></td><td></td></tr><tr><td valign="top"><a href="#stop_manager-1">stop_manager/1</a></td><td>Stops the manager.</td></tr><tr><td valign="top"><a href="#which_managers-0">which_managers/0</a></td><td>Returns a list of the running managers.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(ManagerId::<a href="#type-manager_id">manager_id()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink instance

The default layout is TODO

<a name="start_manager-2"></a>

### start_manager/2 ###

<pre><code>
start_manager(ManagerId::<a href="#type-manager_id">manager_id()</a>, Destinations::[<a href="#type-destination">destination()</a>]) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

Equivalent to [`start_manager(ManagerId, Destinations, [])`](#start_manager-3).

<a name="start_manager-3"></a>

### start_manager/3 ###

<pre><code>
start_manager(ManagerId::<a href="#type-manager_id">manager_id()</a>, Destinations::[<a href="#type-destination">destination()</a>], Options::<a href="#type-options">options()</a>) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

<a name="stop_manager-1"></a>

### stop_manager/1 ###

<pre><code>
stop_manager(ManagerId::<a href="#type-manager_id">manager_id()</a>) -&gt; ok
</code></pre>
<br />

Stops the manager

If the manager associated to `ManagerId` does not exists, it is silently ignored.

<a name="which_managers-0"></a>

### which_managers/0 ###

<pre><code>
which_managers() -&gt; [<a href="#type-manager_id">manager_id()</a>]
</code></pre>
<br />

Returns a list of the running managers

