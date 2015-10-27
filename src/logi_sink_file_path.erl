-module(logi_sink_file_path).

-export([new/2]).
-export([init/1, next_path/1, handle_info/2]).

-export_type([path/0]).

-callback init(path()) -> path().
-callback next_path(path()) -> {ok, binary(), path()} | {error, Reason::term()}.
-callback handle_info(term(), path()) ->
    {ok, boolean(), path()} | {error, term()} | ignore.

-type path() :: term().

new(Module, State) ->
    {Module, State}.

init({Module, State}) ->
    {Module, Module:init(State)}.

next_path({Module, State0}) ->
    case Module:next_path(State0) of
        {error, Reason}    -> {error, Reason};
        {ok, Path, State1} -> {ok, Path, {Module, State1}}
    end.

handle_info(Info, {Module, State0}) ->
    case Module:handle_info(Info, State0) of
        {ok, Rotate, State1} -> {ok, Rotate, {Module, State1}};
        Other                -> Other
    end.
