%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc The supervisor for logi_sink_file_writer processes
%% @private
-module(logi_sink_file_writer_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).
-export([start_child/2, stop_child/1, which_children/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts the supervisor
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Starts a new child process
-spec start_child(logi_sink_file:writer_id(), logi_sink_file_writer:start_arg()) -> {ok, pid()} | {error, Reason::term()}.
start_child(WriterId, Arg) ->
    Child =
        {WriterId, {logi_sink_file_writer, start_link, [WriterId, Arg]}, permanent, 5000, worker, [logi_sink_file_writer]},
    supervisor:start_child(?MODULE, Child).

%% @doc Stops the child process
-spec stop_child(logi_sink_file:writer_id()) -> ok.
stop_child(WriterId) ->
    _ = supervisor:terminate_child(?MODULE, WriterId),
    _ = supervisor:delete_child(?MODULE, WriterId),
    ok.

%% @doc Returns a newly created list of the existing children
-spec which_children() -> [{logi_sink_file:writer_id(), Child}] when Child :: pid() | restarting.
which_children() ->
    [{Id, Child} || {Id, Child, _, _} <- supervisor:which_children(?MODULE)].

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    {ok, { {one_for_one, 1, 5}, []} }.
