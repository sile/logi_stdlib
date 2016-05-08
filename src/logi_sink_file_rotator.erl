%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc The interface of file rotators
%% @end
-module(logi_sink_file_rotator).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).
-export([is_rotator/1]).
-export([rotate/2, get_current_filepath/2, is_outdated/2]).

-export_type([rotator/0]).
-export_type([callback_module/0, state/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback rotate(logi_sink_file:filepath(), state()) ->
    {ok, logi_sink_file:filepath(), state()} | {error, Reason::term()}.

-callback get_current_filepath(logi_sink_file:filepath(), state()) ->
    {ok, logi_sink_file:filepath(), state()} | {error, Reason::term()}.

-callback is_outdated(logi_sink_file:filepath(), state()) ->
    {IsOutdated::boolean(), NextCheckTime::timeout(), state()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque rotator() :: {callback_module(), state()}.
%% A rotator instance

-type callback_module() :: module().
%% A module that implements the `logi_sink_file_rotator' behaviour.

-type state() :: term().
%% The state of an instance of a `logi_sink_file_rotator' implementing module.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(Module, undefined)
-spec new(callback_module()) -> rotator().
new(Module) -> new(Module, undefined).

%% @doc Creates a new rotator instance
-spec new(callback_module(), state()) -> rotator().
new(Module, State) ->
    _ = is_callback_module(Module) orelse error(badarg, [Module, State]),
    {Module, State}.

%% @doc Returns `true' if `X' is a rotator instance, `false' otherwise
-spec is_rotator(X :: (rotator() | term())) -> boolean().
is_rotator({Module, _}) -> is_callback_module(Module);
is_rotator(_)           -> false.

%% @doc Rotates `Filepath'
%%
%% `Rotated' is new file path after the rotation.
%% It may be the same as `FilePath'.
%%
%% A implementation module may not physically rotate the file (e.g., leaves the old file as it is).
-spec rotate(logi_sink_file:filepath(), rotator()) ->
                    {ok, Rotated::logi_sink_file:filepath(), rotator()} | {error, Reason::term()}.
rotate(FilePath, {Module, State0}) ->
    case Module:rotate(FilePath, State0) of
        {error, Reason}       -> {error, Reason};
        {ok, Rotated, State1} -> {ok, Rotated, {Module, State1}}
    end.

%% @doc Gets the current output file path
-spec get_current_filepath(logi_sink_file:filepath(), rotator()) ->
                                  {ok, logi_sink_file:filepath(), rotator()} | {error, Reason::term()}.
get_current_filepath(BaseFilePath, {Module, State0}) ->
    case Module:get_current_filepath(BaseFilePath, State0) of
        {error, Reason}        -> {error, Reason};
        {ok, FilePath, State1} -> {ok, FilePath, {Module, State1}}
    end.

%% @doc Determines the given file path is outdated
%%
%% If `IsOutdated' is `false', the caller process invokes {@link rotate/2} to rotate the old file.
%% Then it will reopen a new file path which is the result of {@link get_current_filepath/2}.
%%
%% The function will be re-invoked after `NextCheckTime' milliseconds.
-spec is_outdated(logi_sink_file:filepath(), rotator()) ->
                                {IsOutdated::boolean(), NextCheckTime::timeout(), rotator()}.
is_outdated(FilePath, {Module, State0}) ->
    {IsOutdated, NextCheckTime, State1} = Module:is_outdated(FilePath, State0),
    {IsOutdated, NextCheckTime, {Module, State1}}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec is_callback_module(callback_module() | term()) -> boolean().
is_callback_module(X) ->
    (is_atom(X) andalso
     logi_utils:function_exported(X, rotate, 2) andalso
     logi_utils:function_exported(X, get_current_filepath, 2) andalso
     logi_utils:function_exported(X, is_outdated, 2)).
