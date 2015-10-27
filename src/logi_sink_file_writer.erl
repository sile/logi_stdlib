%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_sink_file_writer).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/2, write/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records
%%----------------------------------------------------------------------------------------------------------------------
-define(REOPEN_INTERVAL, 60 * 1000). % TODO: option

-define(STATE, ?MODULE).

-record(?STATE,
        {
          fd   :: file:fd(),
          path :: logi_sink_file_path:path()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% TODO: doc
-spec start_link(logi_sink_file:writer_id(), logi_sink_file_path:path()) -> {ok, pid()} | {error, Reason::term()}.
start_link(WriterId, PathGen) ->
    gen_server:start_link({local, WriterId}, ?MODULE, [PathGen], []).

-spec write(logi_sink_file:writer_id(), iodata()) -> ok.
write(WriterId, Message) ->
    gen_server:cast(WriterId, {write, Message}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([PathGen00]) ->
    PathGen0 = logi_sink_file_path:init(PathGen00),
    case open_file(PathGen0) of
        {error, Reason}    -> {stop, Reason};
        {ok, Fd, PathGen1} ->
            State =
                #?STATE{
                    fd   = Fd,
                    path = PathGen1
                   },
            ok = schedule_reopen(),
            {ok, State}
    end.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({write, Arg}, State) ->
    handle_write(Arg, State);
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(reopen, State0) ->
    case reopen_file(State0) of
        {error, Reason} -> {stop, Reason, State0};
        {ok, State1}    ->
            ok = schedule_reopen(),
            {noreply, State1}
    end;
handle_info(Info, State) ->
    case logi_sink_file_path:handle_info(Info, State#?STATE.path) of
        ignore            -> {noreply, State};
        {error, Reason}   -> {stop, Reason, State};
        {ok, false, Path} -> {noreply, State#?STATE{path = Path}};
        {ok, true,  Path} ->
            State1 = State#?STATE{path = Path},
            case reopen_file(State1) of
                {error, Reason} -> {stop, Reason, State1};
                {ok, State2}    -> {noreply, State2}
            end
    end.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec schedule_reopen() -> ok.
schedule_reopen() ->
    _ = erlang:send_after(?REOPEN_INTERVAL, self(), reopen), % TODO: add tag
    ok.

-spec reopen_file(#?STATE{}) -> {ok, #?STATE{}} | {error, Reason::term()}.
reopen_file(State) ->
    ok = file:close(State#?STATE.fd),
    case open_file(State#?STATE.path) of
        {error, Reason}   -> {error, Reason};
        {ok, Fd, PathGen} -> {ok, State#?STATE{fd = Fd, path = PathGen}}
    end.

-spec open_file(logi_sink_file_path:path()) -> {ok, file:fd(), logi_sink_file_path:path()} | {error, Reason::term()}.
open_file(PathGen0) ->
    case logi_sink_file_path:next_path(PathGen0) of
        {error, Reason}      -> {error, Reason};
        {ok, Path, PathGen1} ->
            case filelib:ensure_dir(Path) of
                {error, Reason} -> {error, {cannot_mkdir, Path, Reason}};
                ok              ->
                    %% TODO: customizable file options
                    case file:open(Path, [append, raw, delayed_write, binary]) of
                        {error, Reason} -> {error, {cannot_open_file, Path, Reason}};
                        {ok, Fd}        -> {ok, Fd, PathGen1}
                    end
            end
    end.

-spec handle_write(iodata(), #?STATE{}) -> {noreply, #?STATE{}} | {stop, Reason::term(), #?STATE{}}.
handle_write(Message, State) ->
    %% TODO: correct handling of abnormal cases (ex. diskfull)
    case file:write(State#?STATE.fd, Message) of
        {error, Reason} -> {stop, {cannot_write_file, Reason}, State};
        ok              -> {noreply, State}
    end.
