

# Module logi_filter_and #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

"AND" filter.

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_filter`](logi_filter.md).

<a name="description"></a>

## Description ##


#### <a name="EXAMPLE">EXAMPLE</a> ####


```erlang

  > Filter =
        logi_filter_and:new(
          [
            logi_builtin_filter_fun:new(fun (C) -> info  =:= logi_context:get_severity(C) end),
            logi_builtin_filter_fun:new(fun (C) -> maps:is_key(id, logi_context:get_headers(C)) end)
          ]).
  > logi:save_as_default(logi:new([{filter, Filter}])).
  > logi_channel:install_sink(debug, logi_sink_console:new()).
  > application:set_env(logi, warn_no_parse_transform, false).
  > logi:info("hello world"). % This message is discarded
  > logi:debug("hello world", [], [{headers, #{id => hoge}}]). % This message is discarded
  > logi:info("hello world", [], [{headers, #{id => hoge}}]).
  2015-11-02 13:46:26.272 [info] nonode@nohost <0.98.0> erl_eval:do_apply:673 [id=hoge] hello world
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new filter instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(AndFilters::[<a href="logi_filter.md#type-filter">logi_filter:filter()</a>]) -&gt; <a href="logi_filter.md#type-filter">logi_filter:filter()</a>
</code></pre>
<br />

Creates a new filter instance

The fitler filters messages using the AND boolean operator on the `AndFilters`.
If any filter in `AndFilters` returns `false`, the entire result will become to `false`.

`AndFilters` are applied left to right, and if any filter returns `false`, the remaining filters will not be applied.

