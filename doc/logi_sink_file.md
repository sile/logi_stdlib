

# Module logi_sink_file #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

ファイル出力用のシンク.

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink`](logi_sink.md).

<a name="description"></a>

## Description ##

ログファイルのローテーションに関しては[`logi_sink_file_rotator`](logi_sink_file_rotator.md)の実装モジュールが担当している。
(e.g. 日付単位やサイズ単位のローテーション、古いログファイルの削除、ローテート時の圧縮、etc)


### <a name="NOTE">NOTE</a> ###

このシンク自体には過負荷防止の仕組みはないので本番環境で使用する場合は[`logi_sink_flow_limiter`](logi_sink_flow_limiter.md)等との併用が推奨される。

writerプロセスは、定期的に出力先のパスをチェックし、もしログファイルが存在しない場合は、再度オープンされる。
ただし、例えば上書きされた場合等のように、ファイル(i.e. unixならi-node)が変わっていてもパスが同じ場合は、
その変更は検出されないので注意が必要。(既に存在しないファイルに対してログメッセージの書き込みが行われ続けることになる)

ディスクフルや権限エラー等の理由で、ログファイルのオープンや書き込みが行えなかった場合は、
writerの起動時に指定したロガーを用いて`alert`レベルのエラーが報告される。
その後、writerプロセス自体は(再起動後も同様のエラーとなった場合は)停止するため、
エラー後に、再びログ出力を有効にした場合は、再度[`start_writer/3`](#start_writer-3)を呼び出して、プロセスを起動する必要がある。
(シンクの再インストールは不要)


### <a name="EXAMPLE">EXAMPLE</a> ###


```erlang

  > {ok, _} =  logi_sink_file:start_writer(sample_file_writer, <<"/tmp/sample.log">>).
  > {ok, _} = logi_channel:install_sink(debug, logi_sink_file:new(sample_file_writer)).
  > logi:info("hello world").
  > file:read_file("/tmp/sample.log").
  {ok,<<"2015-11-04 00:13:33.058 [info] nonode@nohost <0.98.0> erl_eval:do_apply:673 [] hello world\n">>}
```

<a name="types"></a>

## Data Types ##




### <a name="type-filepath">filepath()</a> ###


<pre><code>
filepath() = binary()
</code></pre>

 A log file path



### <a name="type-open_options">open_options()</a> ###


<pre><code>
open_options() = list()
</code></pre>

 ログファイルのオープン時に指定するオプション群

詳細は[file:mode/0](http://www.erlang.org/doc/man/file.html#type-mode)のドキュメントを参照のこと



### <a name="type-writer_id">writer_id()</a> ###


<pre><code>
writer_id() = atom()
</code></pre>

 The identifier of a file writer



### <a name="type-writer_option">writer_option()</a> ###


<pre><code>
writer_option() = {logger, <a href="logi.md#type-logger">logi:logger()</a>} | {rotator, <a href="logi_sink_file_rotator.md#type-rotator">logi_sink_file_rotator:rotator()</a>} | {open_opt, <a href="#type-open_options">open_options()</a>}
</code></pre>

`logger`:
- 起動したwriterのログの出力先
- ディスクフル等によりファイル書き込み自体が行いない場合も、ここにログが出力されるので、`error`以上の深刻度のログメッセージは、信頼できる出力先に吐かれるようにしておくことが推奨される
- default: `logi:default_logger()`

`rotator`:
- 起動したwriterが使用する`logi_sink_file_rotator:rotator()`のインスタンス
- ログファイルの実際のパスやローテーションポリシー等は、これによって決定される
- default: `logi_sink_file_rotator_do_nothing:new()`

`open_opt`:
- ログファイルのオープン時に指定するオプション群
- default: `[append, raw, delayed_write]`



### <a name="type-writer_options">writer_options()</a> ###


<pre><code>
writer_options() = [<a href="#type-writer_option">writer_option()</a>]
</code></pre>

 The default options are `[append, raw, delayed_write]` % TODO:

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Equivalent to <a href="#new-2"><tt>new(Writer,
logi_layout_newline:new(logi_layout_limit:new(logi_layout_default:new())))</tt></a>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new sink instance.</td></tr><tr><td valign="top"><a href="#start_writer-2">start_writer/2</a></td><td>Equivalent to <a href="#start_writer-3"><tt>start_writer(WriterId, FilePath, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#start_writer-3">start_writer/3</a></td><td>Starts a new file writer.</td></tr><tr><td valign="top"><a href="#stop_writer-1">stop_writer/1</a></td><td>Stops the file writer.</td></tr><tr><td valign="top"><a href="#which_writers-0">which_writers/0</a></td><td>Returns a list of the running file writers.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Writer::<a href="#type-writer_id">writer_id()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Equivalent to [`new(Writer,logi_layout_newline:new(logi_layout_limit:new(logi_layout_default:new())))`](#new-2).

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Writer::<a href="#type-writer_id">writer_id()</a>, Layout::<a href="logi_layout.md#type-layout">logi_layout:layout</a>(iodata())) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink instance

<a name="start_writer-2"></a>

### start_writer/2 ###

<pre><code>
start_writer(WriterId::<a href="#type-writer_id">writer_id()</a>, FilePath::<a href="#type-filepath">filepath()</a>) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

Equivalent to [`start_writer(WriterId, FilePath, [])`](#start_writer-3).

<a name="start_writer-3"></a>

### start_writer/3 ###

<pre><code>
start_writer(WriterId::<a href="#type-writer_id">writer_id()</a>, FilePath::<a href="#type-filepath">filepath()</a>, Options::<a href="#type-writer_options">writer_options()</a>) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

Starts a new file writer

<a name="stop_writer-1"></a>

### stop_writer/1 ###

<pre><code>
stop_writer(WriterId::<a href="#type-writer_id">writer_id()</a>) -&gt; ok
</code></pre>
<br />

Stops the file writer

If the writer does not exists, it is silently ignored.

<a name="which_writers-0"></a>

### which_writers/0 ###

<pre><code>
which_writers() -&gt; [<a href="#type-writer_id">writer_id()</a>]
</code></pre>
<br />

Returns a list of the running file writers

