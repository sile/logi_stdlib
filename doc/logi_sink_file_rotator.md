

# Module logi_sink_file_rotator #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

ファイルのローテーション機能を提供するモジュール用のインターフェース定義.

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__This module defines the `logi_sink_file_rotator` behaviour.__<br /> Required callback functions: `rotate/2`, `get_current_filepath/2`, `is_outdated/2`.

<a name="types"></a>

## Data Types ##




### <a name="type-callback_module">callback_module()</a> ###


<pre><code>
callback_module() = module()
</code></pre>

 A module that implements the `logi_sink_file_rotator` behaviour.



### <a name="type-rotator">rotator()</a> ###


__abstract datatype__: `rotator()`

 A rotator instance



### <a name="type-state">state()</a> ###


<pre><code>
state() = term()
</code></pre>

 The state of an instance of a `logi_sink_file_rotator` implementing module.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_current_filepath-2">get_current_filepath/2</a></td><td>現在のログファイルの出力先パスを返す.</td></tr><tr><td valign="top"><a href="#is_outdated-2">is_outdated/2</a></td><td>指定されたログファイルのパスが古くないかどうかを判定する.</td></tr><tr><td valign="top"><a href="#is_rotator-1">is_rotator/1</a></td><td>Returns <code>true</code> if <code>X</code> is a rotator instance, <code>false</code> otherwise.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(Module, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new rotator instance.</td></tr><tr><td valign="top"><a href="#rotate-2">rotate/2</a></td><td><code>FilePath</code>のローテーションを行う.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_current_filepath-2"></a>

### get_current_filepath/2 ###

<pre><code>
get_current_filepath(BaseFilePath::<a href="logi_sink_file.md#type-filepath">logi_sink_file:filepath()</a>, X2::<a href="#type-rotator">rotator()</a>) -&gt; {ok, <a href="logi_sink_file.md#type-filepath">logi_sink_file:filepath()</a>, <a href="#type-rotator">rotator()</a>} | {error, Reason::term()}
</code></pre>
<br />

現在のログファイルの出力先パスを返す

<a name="is_outdated-2"></a>

### is_outdated/2 ###

<pre><code>
is_outdated(FilePath::<a href="logi_sink_file.md#type-filepath">logi_sink_file:filepath()</a>, X2::<a href="#type-rotator">rotator()</a>) -&gt; {IsOutdated::boolean(), NextCheckTime::timeout(), <a href="#type-rotator">rotator()</a>}
</code></pre>
<br />

指定されたログファイルのパスが古くないかどうかを判定する

`IsOutdated`が`false`の場合は、writerプロセスは[`rotate/2`](#rotate-2)を呼び出して古いファイルをローテートした上で、
[`get_current_filepath/2`](#get_current_filepath-2)で取得したファイルをオープンし、以降はそのファイルに対してログメッセージの書き込みを行うようになる。

この関数は`NextCheckTime`後に再び呼び出される。

<a name="is_rotator-1"></a>

### is_rotator/1 ###

<pre><code>
is_rotator(X::<a href="#type-rotator">rotator()</a> | term()) -&gt; boolean()
</code></pre>
<br />

Returns `true` if `X` is a rotator instance, `false` otherwise

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>) -&gt; <a href="#type-rotator">rotator()</a>
</code></pre>
<br />

Equivalent to [`new(Module, undefined)`](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Module::<a href="#type-callback_module">callback_module()</a>, State::<a href="#type-state">state()</a>) -&gt; <a href="#type-rotator">rotator()</a>
</code></pre>
<br />

Creates a new rotator instance

<a name="rotate-2"></a>

### rotate/2 ###

<pre><code>
rotate(FilePath::<a href="logi_sink_file.md#type-filepath">logi_sink_file:filepath()</a>, X2::<a href="#type-rotator">rotator()</a>) -&gt; {ok, Rotated::<a href="logi_sink_file.md#type-filepath">logi_sink_file:filepath()</a>, <a href="#type-rotator">rotator()</a>} | {error, Reason::term()}
</code></pre>
<br />

`FilePath`のローテーションを行う

ローテート結果のファイルパスは`Rotated`として返される。

なお、実際にローテーションを行うかどうかは、ビヘイビアの実装モジュール任せとなる。
(行われなかった場合は、`FilePath`と`Rotated`が等しくなる)

