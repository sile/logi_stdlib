%% @copyright 2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A noop logi_sink_file_rotator implementation
%% @end
-module(logi_sink_file_rotator_noop).

-behaviour(logi_sink_file_rotator).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_file_rotator' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([rotate/2, get_current_filepath/2, is_outdated/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new rotator instance
-spec new() -> logi_sink_file_rotator:rotator().
new() -> logi_sink_file_rotator:new(?MODULE).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_file_rotator' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
rotate(FilePath, State) -> {ok, FilePath, State}.

%% @private
get_current_filepath(FilePath, State) -> {ok, FilePath, State}.

%% @private
is_outdated(_FilePath, State) -> {false, infinity, State}.
