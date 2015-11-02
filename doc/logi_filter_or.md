

# Module logi_filter_or #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

"OR" filter.

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_filter`](logi_filter.md).

<a name="description"></a>

## Description ##


#### <a name="EXAMPLE">EXAMPLE</a> ####


```erlang

  > Filter =
        logi_filter_or:new(
          [
            %% 'info' or 'alert' messages are allowed
            logi_builtin_filter_fun:new(fun (C) -> info  =:= logi_context:get_severity(C) end),
            logi_builtin_filter_fun:new(fun (C) -> alert =:= logi_context:get_severity(C) end)
          ]).
  > logi:save_as_default(logi:new([{filter, Filter}])).
  > logi_channel:install_sink(debug, logi_sink_console:new()).
  > application:set_env(logi, warn_no_parse_transform, false).
  > logi:debug("hello world"). % This message is discarded
  > logi:info("hello world").
  2015-11-02 13:24:02.847 [info] nonode@nohost <0.98.0> erl_eval:do_apply:673 [] hello world
  > logi:notice("hello world"). % This message is discarded
  > logi:alert("hello world").
  2015-11-02 13:25:05.222 [alert] nonode@nohost <0.98.0> erl_eval:do_apply:673 [] hello world
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new filter instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(OrFilters::[<a href="logi_filter.md#type-filter">logi_filter:filter()</a>]) -&gt; <a href="logi_filter.md#type-filter">logi_filter:filter()</a>
</code></pre>
<br />

Creates a new filter instance

The fitler filters messages using the OR boolean operator on the `OrFilters`.
If any filter in `OrFilters` returns `true`, the entire result will become to `true`.

`OrFilters` are applied left to right, and if any filter returns `true`, the remaining filters will not be applied.

