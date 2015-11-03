%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ファイル出力用のシンク
%%
%% ログファイルのローテーションに関しては{@link logi_sink_file_rotator}の実装モジュールが担当している。
%% (e.g. 日付単位やサイズ単位のローテーション、古いログファイルの削除、ローテート時の圧縮、etc)
%%
%% == NOTE ==
%% このシンク自体には過負荷防止の仕組みはないので本番環境で使用する場合は{@link logi_sink_flow_limiter}等との併用が推奨される。
%%
%% writerプロセスは、定期的に出力先のパスをチェックし、もしログファイルが存在しない場合は、再度オープンされる。
%% ただし、例えば上書きされた場合等のように、ファイル(i.e. unixならi-node)が変わっていてもパスが同じ場合は、
%% その変更は検出されないので注意が必要。(既に存在しないファイルに対してログメッセージの書き込みが行われ続けることになる)
%%
%% ディスクフルや権限エラー等の理由で、ログファイルのオープンや書き込みが行えなかった場合は、
%% writerの起動時に指定したロガーを用いて`alert'レベルのエラーが報告される。
%% その後、writerプロセス自体は(再起動後も同様のエラーとなった場合は)停止するため、
%% エラー後に、再びログ出力を有効にした場合は、再度{@link start_writer/3}を呼び出して、プロセスを起動する必要がある。
%% (シンクの再インストールは不要)
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > {ok, _} =  logi_sink_file:start_writer(sample_file_writer, &lt;&lt;"/tmp/sample.log"&gt;&gt;).
%% > {ok, _} = logi_channel:install_sink(debug, logi_sink_file:new(sample_file_writer)).
%% > logi:info("hello world").
%% > file:read_file("/tmp/sample.log").
%% {ok,&lt;&lt;"2015-11-04 00:13:33.058 [info] nonode@nohost &lt;0.98.0&gt; erl_eval:do_apply:673 [] hello world\n"&gt;&gt;}
%% </pre>
-module(logi_sink_file).

-behaviour(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1]).

-export([start_writer/2, start_writer/3]).
-export([stop_writer/1]).
-export([which_writers/0]).

-export_type([filepath/0]).
-export_type([open_options/0]).

-export_type([writer_id/0]).
-export_type([writer_options/0, writer_option/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/5, default_layout/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type filepath() :: binary().
%% A log file path

-type open_options() :: list().
%% ログファイルのオープン時に指定するオプション群
%%
%% 詳細は[file:mode/0](http://www.erlang.org/doc/man/file.html#type-mode)のドキュメントを参照のこと

-type writer_id() :: atom().
%% The identifier of a file writer

-type writer_options() :: [writer_option()].
%% The default options are `[append, raw, delayed_write]' % TODO:

-type writer_option() :: {logger, logi:logger()}
                       | {rotator, logi_sink_file_rotator:rotator()}
                       | {open_opt, open_options()}.
%% `logger':
%% - 起動したwriterのログの出力先
%% - ディスクフル等によりファイル書き込み自体が行いない場合も、ここにログが出力されるので、`error'以上の深刻度のログメッセージは、信頼できる出力先に吐かれるようにしておくことが推奨される
%% - default: `logi:default_logger()'
%%
%% `rotator':
%% - 起動したwriterが使用する`logi_sink_file_rotator:rotator()'のインスタンス
%% - ログファイルの実際のパスやローテーションポリシー等は、これによって決定される
%% - default: `logi_sink_file_rotator_do_nothing:new()'
%%
%% `open_opt':
%% - ログファイルのオープン時に指定するオプション群
%% - default: `[append, raw, delayed_write]'

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new sink instance
%%
%% The default layout of the sink is
%% `logi_layout_newline:new(logi_layout_limie:new(logi_layout_default:new()))'.
-spec new(writer_id()) -> logi_sink:sink().
new(Writer) ->
    _ = is_atom(Writer) orelse error(bagarg, [Writer]),
    logi_sink:new(?MODULE, Writer).

%% @equiv start_writer(WriterId, FilePath, [])
-spec start_writer(writer_id(), filepath()) -> {ok, pid()} | {error, Reason::term()}.
start_writer(WriterId, FilePath) ->
    start_writer(WriterId, FilePath, []).

%% @doc Starts a new file writer
-spec start_writer(writer_id(), filepath(), writer_options()) -> {ok, pid()} | {error, Reason::term()}.
start_writer(WriterId, FilePath, Options) ->
    Args = [WriterId, FilePath, Options],
    _ = is_atom(WriterId) orelse error(badarg, Args),
    _ = is_binary(FilePath) orelse error(badarg, Args),
    _ = is_list(Options) orelse error(badarg, Args),

    Logger = proplists:get_value(logger, Options, logi:default_logger()),
    Rotator = proplists:get_value(rotator, Options, logi_sink_file_rotator_do_nothing:new()),
    OpenOpt = proplists:get_value(open_opt, Options, [append, raw, delayed_write]),
    _ = logi:is_logger(Logger) orelse error(badarg, Args),
    _ = logi_sink_file_rotator:is_rotator(Rotator) orelse error(badarg, Args),
    _ = is_list(OpenOpt) orelse error(badarg, OpenOpt),

    logi_sink_file_writer_sup:start_child(WriterId, {FilePath, Logger, Rotator, OpenOpt}).

%% @doc Stops the file writer
%%
%% If the writer does not exists, it is silently ignored.
-spec stop_writer(writer_id()) -> ok.
stop_writer(WriterId) ->
    _ = is_atom(WriterId) orelse error(badarg, [WriterId]),
    logi_sink_file_writer_sup:stop_child(WriterId).

%% @doc Returns a list of the running file writers
-spec which_writers() -> [writer_id()].
which_writers() ->
    [Id || {Id, _} <- logi_sink_file_writer_sup:which_children()].

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Layout, Format, Data, Writer) ->
    FormattedData = logi_layout:format(Context, Format, Data, Layout),
    logi_sink_file_writer:write(Writer, FormattedData).

%% @private
default_layout(_Writer) ->
    logi_layout_newline:new(
      logi_layout_limit:new(
        logi_layout_default:new())).
