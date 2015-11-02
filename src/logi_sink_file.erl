%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO: logi_sink_tcp
%%
%% @doc TODO
-module(logi_sink_file).

-behaviour(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_writer/2, stop_writer/1]).
-export([new/1]).

-export_type([writer_id/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/5, default_layout/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type writer_id() :: atom().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% TODO: doc
-spec start_writer(writer_id(), logi_sink_file_path:path()) -> {ok, MayChange::pid()} | {error, Reason::term()}.
start_writer(WriterId, PathGen) ->
    logi_sink_file_writer_sup:start_child(WriterId, PathGen).

-spec stop_writer(writer_id()) -> ok.
stop_writer(WriterId) ->
    logi_sink_file_writer_sup:stop_child(WriterId).

%% @doc Creates a new sink instance
%%
%% The default layout of the sink is `logi_layout_limit:new(logi_layout_default:new())'.
-spec new(writer_id()) -> logi_sink:sink().
new(Writer) ->
    _ = is_atom(Writer) orelse error(bagarg, [Writer]),
    logi_sink:new(?MODULE, Writer).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Layout, Format, Data, Writer) ->
    IoData = logi_layout:format(Context, Format, Data, Layout),
    logi_sink_file_writer:write(Writer, IoData).

%% @private
default_layout(_Writer) ->
    logi_layout_limit:new(logi_layout_default:new()).
