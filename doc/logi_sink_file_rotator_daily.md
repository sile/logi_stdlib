

# Module logi_sink_file_rotator_daily #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

日にち単位でログファイルのローテーションを行うための`logi_sink_file_rotator`の実装モジュール.

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink_file_rotator`](logi_sink_file_rotator.md).

<a name="description"></a>

## Description ##

[`logi_sink_file:start_writer/3`](logi_sink_file.md#start_writer-3)の第二引数で渡したファイルパスは、以下のルールに従い展開される:
- パス中の文字列`{YY}`は、現在の年の下二桁に置換される
- パス中の文字列`{YYYY}`は、現在の年で置換される (四桁に足りない場合は`0`でパディング)
- パス中の文字列`{MM}`は、現在の月で置換される (二桁に足りない場合は`0`でパディング)
- パス中の文字列`{DD}`は、現在の日で置換される (二桁に足りない場合は`0`でパディング)

このモジュールが担当するのは、上記ルールに基づくログファイルパスの展開と、
日を跨いだタイミングでのローテーションの実施、のみで実際のローテーション処理(e.g. ファイルを圧縮するかどうか)は、
他の`logi_sink_file_rotator`の実装モジュールに委譲されている。


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > Rotator = logi_sink_file_rotator_daily:new().
  > {ok, _} = logi_sink_file:start_writer(sample_file_writer, <</tmp/{YYYY}-{MM}-{DD}-sample.log">>, [{rotator, Rotator}]).
  > {ok, _} = logi_channel:install_sink(debug, logi_sink_file:new(sample_file_writer)).
  > logi:info("hello world").
  > file:read_file("/tmp/2015-11-04-sample.log").
  {ok,<<"2015-11-04 00:47:39.105 [info] nonode@nohost <0.114.0> erl_eval:do_apply:673 [] hello world\n">>}
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Equivalent to <a href="#new-1"><tt>new(logi_sink_file_rotator_do_nothing:new())</tt></a>.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new rotator instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="logi_sink_file_rotator.md#type-rotator">logi_sink_file_rotator:rotator()</a>
</code></pre>
<br />

Equivalent to [`new(logi_sink_file_rotator_do_nothing:new())`](#new-1).

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(BaseRotator::<a href="logi_sink_file_rotator.md#type-rotator">logi_sink_file_rotator:rotator()</a>) -&gt; <a href="logi_sink_file_rotator.md#type-rotator">logi_sink_file_rotator:rotator()</a>
</code></pre>
<br />

Creates a new rotator instance

