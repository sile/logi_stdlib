%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%% @end
%%
%% 日にち単位でログファイルのローテーションを行うための`logi_sink_file_rotator'の実装モジュール
%%
%% {@link logi_sink_file:start_writer/3}の第二引数で渡したファイルパスは、以下のルールに従い展開される:
%% - パス中の文字列`{YY}'は、現在の年の下二桁に置換される
%% - パス中の文字列`{YYYY}'は、現在の年で置換される (四桁に足りない場合は`0'でパディング)
%% - パス中の文字列`{MM}`は、現在の月で置換される (二桁に足りない場合は`0'でパディング)
%% - パス中の文字列`{DD}`は、現在の日で置換される (二桁に足りない場合は`0'でパディング)
%%
%% このモジュールが担当するのは、上記ルールに基づくログファイルパスの展開と、
%% 日を跨いだタイミングでのローテーションの実施、のみで実際のローテーション処理(e.g. ファイルを圧縮するかどうか)は、
%% 他の`logi_sink_file_rotator'の実装モジュールに委譲されている。
%%
%% == EXAMPLE ==
%% <pre lang="erlang">
%% > Rotator = logi_sink_file_rotator_daily:new().
%% > {ok, _} = logi_sink_file:start_writer(sample_file_writer, &lt;&lt;/tmp/{YYYY}-{MM}-{DD}-sample.log"&gt;&gt;, [{rotator, Rotator}]).
%% > {ok, _} = logi_channel:install_sink(debug, logi_sink_file:new(sample_file_writer)).
%% > logi:info("hello world").
%% > file:read_file("/tmp/2015-11-04-sample.log").
%% {ok,&lt;&lt;"2015-11-04 00:47:39.105 [info] nonode@nohost &lt;0.114.0&gt; erl_eval:do_apply:673 [] hello world\n"&gt;&gt;}
%% </pre>
-module(logi_sink_file_rotator_daily).

-behaviour(logi_sink_file_rotator).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0, new/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_file_rotator' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([rotate/2, get_current_filepath/2, is_outdated/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(logi_sink_file_rotator_noop:new())
-spec new() -> logi_sink_file_rotator:rotator().
new() -> new(logi_sink_file_rotator_noop:new()).

%% @doc Creates a new rotator instance
-spec new(logi_sink_file_rotator:rotator()) -> logi_sink_file_rotator:rotator().
new(BaseRotator) ->
    _ = logi_sink_file_rotator:is_rotator(BaseRotator) orelse error(badarg, [BaseRotator]),
    {Today, _} = calendar:local_time(),
    logi_sink_file_rotator:new(?MODULE, {Today, BaseRotator}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_file_rotator' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
rotate(FilePath, {Today, BaseRotator0}) ->
    case logi_sink_file_rotator:rotate(FilePath, BaseRotator0) of
        {error, Reason}             -> {error, Reason};
        {ok, Rotated, BaseRotator1} -> {ok, Rotated, {Today, BaseRotator1}}
    end.

%% @private
get_current_filepath(FilePath0, State = {Today, _}) ->
    {Y, M, D} = Today,
    FilePath1 = re:replace(FilePath0, "{YYYY}", io_lib:format("~4..0B", [Y]), [global, {return, binary}]),
    FilePath2 = re:replace(FilePath1, "{YY}",   io_lib:format("~2..0B", [Y]), [global, {return, binary}]),
    FilePath3 = re:replace(FilePath2, "{MM}",   io_lib:format("~2..0B", [M]), [global, {return, binary}]),
    FilePath4 = re:replace(FilePath3, "{DD}",   io_lib:format("~2..0B", [D]), [global, {return, binary}]),
    {ok, FilePath4, State}.

%% @private
is_outdated(_FilePath, State = {CurrentDate, _}) ->
    {Today, Time} = calendar:local_time(),
    {Today =/= CurrentDate, milliseconds_until_tomorrow(Time), setelement(1, State, Today)}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec milliseconds_until_tomorrow(calendar:time()) -> non_neg_integer().
milliseconds_until_tomorrow(Time) ->
    Current = calendar:time_to_seconds(Time),
    Margin = 100,
    Delta = (24 * 60 * 60) - Current,
    Delta * 1000 + Margin.
