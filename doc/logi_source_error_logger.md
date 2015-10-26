

# Module logi_source_error_logger #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

TODO.

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`gen_event`](gen_event.md).

<a name="types"></a>

## Data Types ##




### <a name="type-error_logger_event">error_logger_event()</a> ###


<pre><code>
error_logger_event() = term()
</code></pre>

TODO

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default_log_fun-2">default_log_fun/2</a></td><td></td></tr><tr><td valign="top"><a href="#install-0">install/0</a></td><td></td></tr><tr><td valign="top"><a href="#install-1">install/1</a></td><td></td></tr><tr><td valign="top"><a href="#uninstall-0">uninstall/0</a></td><td></td></tr></table>


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

<a name="install-1"></a>

### install/1 ###

`install(Options) -> any()`

<a name="uninstall-0"></a>

### uninstall/0 ###

`uninstall() -> any()`

