%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc The root supervisor module
%% @private
-module(logi_stdlib_sup).

-behaviour(supervisor).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts the root supervisor
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%----------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([]) ->
    Supervisor = fun (Module) -> {Module, {Module, start_link, []}, permanent, infinity, supervisor, [Module]} end,
    Children =
        [
%%         Supervisor(logi_sink_tcp_writer_sup),
         Supervisor(logi_sink_file_writer_sup),
         Supervisor(logi_sink_flow_limiter_server_sup)
        ],
    {ok, { {one_for_one, 1, 5}, Children} }.
