%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A sink which writes log messages to a file
%%
%% == NOTE ==
%%
%% The sink has no overload protections,
%% so it is recommended to use it together with (for example) {@link logi_slink_flow_limiter}
%% in a production environment.
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > {ok, _} =  logi_sink_file:start_writer(sample_file_writer, &lt;&lt;"/tmp/sample.log"&gt;&gt;).
%% > {ok, _} = logi_channel:install_sink(debug, logi_sink_file:new(sample_file_writer)).
%% > logi:info("hello world").
%% > file:read_file("/tmp/sample.log").
%% {ok,&lt;&lt;"2015-11-04 00:13:33.058 [info] nonode@nohost &lt;0.98.0&gt; erl_eval:do_apply:673 [] hello world\n"&gt;&gt;}
%% </pre>
%% @end
%%
%% TODO: 以下を英訳して、Edocドキュメントに含める
%%
%% ログファイルのローテーションに関しては{@link logi_sink_file_rotator}の実装モジュールが担当している。
%% (e.g. 日付単位やサイズ単位のローテーション、古いログファイルの削除、ローテート時の圧縮、etc)
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
-module(logi_sink_file).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/2, new/3]).
-export([default_layout/0]).

-export_type([options/0, option/0]).
-export_type([open_options/0]).
-export_type([filepath/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type filepath() :: binary().
%% A log file path

-type options() :: [option()].

-type option() :: {layout, logi_layout:layout()}
                | {logger, logi:logger()}
                | {rotator, logi_sink_file_rotator:rotator()}
                | {open_opt, open_options()}.
%% `layout':
%% - The layout instance used by the sink
%% - Default: `logi_sink_file:default_layout()'
%%
%% `logger':
%% - The logger instance which is used to report internal events of the sink process
%% - Default: `logi:default_logger()'
%%
%% `rotator':
%% - The rotator instance used by the sink
%% - Default: `logi_sink_file_rotator_noop:new()'
%%
%% `open_opt':
%% - Log file open options (i.e., the second argument of `file:open/2')
%% - Default: `[append, raw, delayed_write]'

-type open_options() :: list().
%% Log file open options
%%
%% See [file:mode/0](http://www.erlang.org/doc/man/file.html#type-mode) for more details

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(SinkId, FilePath, [])
-spec new(logi_sink:id(), file:name_all()) -> logi_sink:sink().
new(SinkId, FilePath) ->
    new(SinkId, FilePath, []).

%% @doc Creates a new sink
-spec new(logi_sink:id(), file:name_all(), options()) -> logi_sink:sink().
new(SinkId, FilePath, Options) ->
    _ = is_list(Options) orelse error(badarg, [SinkId, FilePath, Options]),

    Layout = proplists:get_value(layout, Options, default_layout()),
    Logger = proplists:get_value(logger, Options, logi:default_logger()),
    Rotator = proplists:get_value(rotator, Options, logi_sink_file_rotator_noop:new()),
    OpenOpt = proplists:get_value(open_opt, Options, [append, raw, delayed_write]),
    _ = logi:is_logger(Logger) orelse error(badarg, [FilePath, Options]),
    _ = logi_sink_file_rotator:is_rotator(Rotator) orelse error(badarg, [SinkId, FilePath, Options]),
    _ = is_list(OpenOpt) orelse error(badarg, [FilePath, Options]),

    FilePathBin = filename:join(FilePath, <<"">>),
    StartArg = {FilePathBin, Logger, Rotator, OpenOpt, Layout},
    logi_sink:new(#{id => SinkId, start => {logi_sink_file_writer, start_link, [StartArg]}}).

%% @doc Default layout
%%
%% `Layout' is `logi_layout_newline:new(logi_layout_limit:new(logi_layout_default:new()))'.
-spec default_layout() -> Layout :: logi_layout:layout().
default_layout() ->
    logi_layout_newline:new(logi_layout_limit:new(logi_layout_default:new())).
