

# Module logi_sink_console #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

コンソール出力用のシンク.

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink`](logi_sink.md).

<a name="description"></a>

## Description ##


### <a name="NOTE">NOTE</a> ###

このシンク自体には過負荷防止の仕組みはないので本番環境で使用する場合は[`logi_sink_flow_limiter`](logi_sink_flow_limiter.md)等との併用が推奨される。
また調査時に一時的にログを出力したいだけなら`logi_channel:install_sink_option/0`の`lifetime`オプションの指定を検討しても良い。


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > logi_channel:install_sink(info, logi_sink_console:new()).
  > logi:info("hello world").
  2015-11-03 10:58:59.920 [info] nonode@nohost <0.113.0> erl_eval:do_apply:673 [] hello world
```

別のレイアウトで出力する:

```erlang

  > Layout = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format(Format, Data) end),
  > logi_channel:install_sink(info, logi_sink_console:new(), [{layout, Layout}, {if_exists, supersede}]).
  > logi:info("hello world").
  hello world
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Creates a new sink instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink instance

The default layout of the sink is
`logi_layout_newline:new(logi_layout_color:new(logi_layout_limie:new(logi_layout_default:new())))`.

