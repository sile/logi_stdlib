

# Module logi_source_error_logger #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`gen_event`](gen_event.md).

<a name="types"></a>

## Data Types ##




### <a name="type-error_logger_event">error_logger_event()</a> ###


<pre><code>
error_logger_event() = {error, <a href="#type-group_leader">group_leader()</a>, {pid(), <a href="io.md#type-format">io:format()</a>, <a href="logi_layout.md#type-data">logi_layout:data()</a>}} | {error_report, <a href="#type-group_leader">group_leader()</a>, {pid(), std_error, Report::term()}} | {error_report, <a href="#type-group_leader">group_leader()</a>, {pid(), Type::term(), Report::term()}} | {warning_msg, <a href="#type-group_leader">group_leader()</a>, {pid(), <a href="io.md#type-format">io:format()</a>, <a href="logi_layout.md#type-data">logi_layout:data()</a>}} | {warning_report, <a href="#type-group_leader">group_leader()</a>, {pid(), std_warning, Report::term()}} | {warning_report, <a href="#type-group_leader">group_leader()</a>, {pid(), Type::term(), Report::term()}} | {info_msg, <a href="#type-group_leader">group_leader()</a>, {pid(), <a href="io.md#type-format">io:format()</a>, <a href="logi_layout.md#type-data">logi_layout:data()</a>}} | {info_report, <a href="#type-group_leader">group_leader()</a>, {pid(), std_info, Report::term()}} | {info_report, <a href="#type-group_leader">group_leader()</a>, {pid(), Type::term(), Report::term()}}
</code></pre>



### <a name="type-group_leader">group_leader()</a> ###


<pre><code>
group_leader() = pid()
</code></pre>




### <a name="type-log_fun">log_fun()</a> ###


<pre><code>
log_fun() = fun((<a href="#type-error_logger_event">error_logger_event()</a>, <a href="logi.md#type-logger_instance">logi:logger_instance()</a>) -&gt; <a href="logi.md#type-logger_instance">logi:logger_instance()</a>)
</code></pre>



### <a name="type-option">option()</a> ###


<pre><code>
option() = {logger, <a href="logi.md#type-logger">logi:logger()</a>} | {forward_logger, <a href="logi.md#type-logger">logi:logger()</a>} | {max_message_queue_len, non_neg_integer()} | {log_fun, <a href="#type-log_fun">log_fun()</a>}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default_log_fun-2">default_log_fun/2</a></td><td></td></tr><tr><td valign="top"><a href="#install-0">install/0</a></td><td>Equivalent to <a href="#install-1"><tt>install([])</tt></a>.</td></tr><tr><td valign="top"><a href="#install-1">install/1</a></td><td></td></tr><tr><td valign="top"><a href="#uninstall-0">uninstall/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default_log_fun-2"></a>

### default_log_fun/2 ###

<pre><code>
default_log_fun(Event::<a href="#type-error_logger_event">error_logger_event()</a>, Logger::<a href="logi.md#type-logger_instance">logi:logger_instance()</a>) -&gt; <a href="logi.md#type-logger_instance">logi:logger_instance()</a>
</code></pre>
<br />

<a name="install-0"></a>

### install/0 ###

`install() -> any()`

Equivalent to [`install([])`](#install-1).

<a name="install-1"></a>

### install/1 ###

<pre><code>
install(Options::<a href="#type-options">options()</a>) -&gt; ok | {error, Reason::term()}
</code></pre>
<br />

<a name="uninstall-0"></a>

### uninstall/0 ###

<pre><code>
uninstall() -&gt; ok | {error, Reason::term()}
</code></pre>
<br />

