%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A sink process and writer for logi_sink_file module
%% @private
%% @end
-module(logi_sink_file_writer).

-behaviour(logi_sink_writer).
-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).

-export_type([start_arg/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/4, get_writee/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(FILE_EXISTENCE_CHECK_INTERVAL, (10 * 1000)).

-define(STATE, ?MODULE).
-record(?STATE,
        {
          fd               :: file:fd(),
          base_filepath    :: logi_sink_file:filepath(),
          current_filepath :: logi_sink_file:filepath(),
          rotator          :: logi_sink_file_rotator:rotator(),
          open_options     :: logi_sink_file:open_options()
        }).

-type start_arg() :: {logi_sink_file:filepath(), logi:logger(), logi_sink_file_rotator:rotator(),
                      logi_sink_file:open_options(), logi_layout:layout()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a new file agent
-spec start_link(start_arg()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Format, Data, {Writee, Layout}) ->
    FormattedData = logi_layout:format(Context, Format, Data, Layout),
    ok = gen_server:cast(Writee, {write, FormattedData}),
    FormattedData.

%% @private
get_writee({Writee, _}) ->
    Writee.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init({BaseFilePath, Logger, Rotator0, OpenOptions, Layout}) ->
    _ = logi:save_as_default(Logger),
    case open_new_file(BaseFilePath, Rotator0, OpenOptions) of
        {error, Reason} ->
            _ = logi:alert("Can't open a log file: reason=~p", [Reason]),
            {stop, Reason};
        {ok, Fd, CurrentFilePath, Rotator1} ->
            _ = logi:info("Started: filepath=~s, rotator=~p", [CurrentFilePath, Rotator1]),
            State =
                #?STATE{
                    fd               = Fd,
                    base_filepath    = BaseFilePath,
                    current_filepath = CurrentFilePath,
                    rotator          = Rotator1,
                    open_options     = OpenOptions
                   },
            ok = schedule_file_existence_check(),
            ok = schedule_rotation_check(0),
            ok = logi_sink_proc:send_writer_to_parent(logi_sink_writer:new(?MODULE, {self(), Layout})),
            {ok, State}
    end.

%% @private
handle_call(_Request, _From, State) -> {noreply, State}.

%% @private
handle_cast({write, Arg}, State) -> handle_write(Arg, State);
handle_cast(_Request, State)     -> {noreply, State}.

%% @private
handle_info(file_existence_check, State) -> handle_file_existence_check(State);
handle_info(rotation_check,       State) -> handle_rotation_check(State);
handle_info(_Info,                State) -> {noreply, State}.

%% @private
terminate(Reason, _State) ->
    _ = logi:info("Terminated: reason=~p", [Reason]),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec schedule_file_existence_check() -> ok.
schedule_file_existence_check() ->
    _ = erlang:send_after(?FILE_EXISTENCE_CHECK_INTERVAL, self(), file_existence_check),
    ok.

-spec schedule_rotation_check(timeout()) -> ok.
schedule_rotation_check(infinity) ->
    ok;
schedule_rotation_check(Time) ->
    _ = erlang:send_after(Time, self(), rotation_check),
    ok.

-spec handle_write(iodata(), #?STATE{}) -> {noreply, #?STATE{}} | {stop, Reason::term(), #?STATE{}}.
handle_write(Message, State) ->
    case file:write(State#?STATE.fd, Message) of
        ok              -> {noreply, State};
        {error, Reason} ->
            _ = logi:alert("Can't write log messages: file=~s, reason=~p", [State#?STATE.current_filepath, Reason]),
            {stop, {Reason, {file, write, [State#?STATE.fd, Message]}}, State}
    end.

-spec handle_file_existence_check(#?STATE{}) -> {noreply, #?STATE{}} | {stop, Reason::term(), #?STATE{}}.
handle_file_existence_check(State0 = #?STATE{current_filepath = FilePath}) ->
    Result =
        case filelib:is_regular(FilePath) of
            true  -> {noreply, State0};
            false ->
                _ = logi:info("The log file is missing: file=~s", [FilePath]),
                case reopen_current_file(State0) of
                    {error, Reason} ->
                        _ = logi:alert("Can't reopen the log file: file=~s, reason=~p", [FilePath, Reason]),
                        {stop, Reason, State0};
                    {ok, State1}    ->
                        _ = logi:info("The log file is reopened: file=~s", [FilePath]),
                        {noreply, State1}
                end
        end,
    ok = schedule_file_existence_check(),
    Result.

-spec handle_rotation_check(#?STATE{}) -> {noreply, #?STATE{}} | {stop, Reason::term(), #?STATE{}}.
handle_rotation_check(State0 = #?STATE{current_filepath = FilePath}) ->
    {IsOutdated, NextCheckTime, Rotator} =
        logi_sink_file_rotator:is_outdated(FilePath, State0#?STATE.rotator),
    State1 = State0#?STATE{rotator = Rotator},
    Result =
        case IsOutdated of
            false -> {noreply, State1};
            true  ->
                _ = logi:info("The log file is outdated: file=~s", [FilePath]),
                case rotate_and_reopen_file(State1) of
                    {error, Reason} ->
                        _ = logi:alert("Can't reopen an up-to-date log file: reason=~p", [Reason]),
                        {stop, Reason, State1};
                    {ok, RotatedFilePath, State2} ->
                        _ = RotatedFilePath =:= FilePath orelse
                            logi:info("The old log file is rotated: from=~p, to=~p", [FilePath, RotatedFilePath]),
                        _ = logi:info("A new log file is opened: file=~s", [State2#?STATE.current_filepath]),
                        {noreply, State2}
                end
        end,
    ok = schedule_rotation_check(NextCheckTime),
    Result.

-spec open_file(logi_sink_file:filepath(), logi_sink_file:open_options()) -> {ok, file:fd()} | {error, Reason::term()}.
open_file(FilePath, Options) ->
    case filelib:ensure_dir(FilePath) of
        {error, Reason} -> {error, {Reason, {filelib, ensure_dir, [FilePath]}}};
        ok              ->
            case file:open(FilePath, Options) of
                {error, Reason} -> {error, {Reason, {file, open, [FilePath, Options]}}};
                {ok, Fd}        -> {ok, Fd}
            end
    end.

-spec open_new_file(logi_sink_file:filepath(), logi_sink_file_rotator:rotator(), logi_sink_file:open_options()) ->
                           {ok, file:fd(), logi_sink_file:filepath(), logi_sink_file_rotator:rotator()} | {error, Reason::term()}.
open_new_file(BaseFilePath, Rotator0, OpenOptions) ->
    case logi_sink_file_rotator:get_current_filepath(BaseFilePath, Rotator0) of
        {error, Reason} ->
            {stop, {Reason, {logi_sink_file_rotator, get_current_filepath, [BaseFilePath, Rotator0]}}};
        {ok, FilePath, Rotator1} ->
            case open_file(FilePath, OpenOptions) of
                {error, Reason} -> {error, Reason};
                {ok, Fd}        -> {ok, Fd, FilePath, Rotator1}
            end
    end.

-spec reopen_current_file(#?STATE{}) -> {ok, #?STATE{}} | {error, Reason::term()}.
reopen_current_file(State) ->
    case file:close(State#?STATE.fd) of
        {error, Reason} -> {error, {Reason, {file, close, [State#?STATE.fd]}}};
        ok              ->
            case open_file(State#?STATE.current_filepath, State#?STATE.open_options) of
                {error, Reason} -> {error, Reason};
                {ok, Fd}        -> {ok, State#?STATE{fd = Fd}}
            end
    end.

-spec rotate_and_reopen_file(#?STATE{}) -> {ok, logi_sink_file:filepath(), #?STATE{}} | {error, Reason::term()}.
rotate_and_reopen_file(State0 = #?STATE{current_filepath = OldFilePath, base_filepath = BaseFilePath}) ->
    case file:close(State0#?STATE.fd) of
        {error, Reason} -> {error, {Reason, {file, close, [State0#?STATE.fd]}}};
        ok              ->
            case logi_sink_file_rotator:rotate(OldFilePath, State0#?STATE.rotator) of
                {error, Reason} ->
                    {error, {Reason, {logi_sink_file_rotator, rotate, [OldFilePath, State0#?STATE.rotator]}}};
                {ok, RotatedFilePath, Rotator0} ->
                    case open_new_file(BaseFilePath, Rotator0, State0#?STATE.open_options) of
                        {error, Reason}                 -> {error, Reason};
                        {ok, Fd, NewFilePath, Rotator1} ->
                            State1 = State0#?STATE{fd = Fd, current_filepath = NewFilePath, rotator = Rotator1},
                            {ok, RotatedFilePath, State1}
                    end
            end
    end.
