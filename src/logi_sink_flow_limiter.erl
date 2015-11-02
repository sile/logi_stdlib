%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO: logi_layout_raw
%%
%% TODO: doc
%%
%% TODO: sinkを連鎖するのはいろいろと不自然なので、別の形にしたい
%%
%% - message_queue_len
%% - bps
-module(logi_sink_flow_limiter).

-behaviour(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_limiter/1, stop_limiter/1]).
-export([new/3]).

-export_type([id/0]).
-export_type([options/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/5, default_layout/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(STATE, ?MODULE).

-record(?STATE,
        {
          limiter               :: id(),
          sink                  :: logi_sink:sink(),
          destination           :: pid() | atom(),
          max_message_queue_len :: pos_integer(),
          max_bitrate           :: pos_integer(),
          period                :: timeout()
        }).

-type id() :: atom().
-type options() :: Todo::term().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec start_limiter(id()) -> {ok, pid()} | {error, Reason::term()}. % XXX: pid() is returned for debugging purposes only
start_limiter(Id) ->
    logi_sink_flow_limiter_server_sup:start_child(Id, []).

-spec stop_limiter(id()) -> ok.
stop_limiter(Id) ->
    logi_sink_flow_limiter_server_sup:stop_child(Id).

%% @doc Creates a new sink instance
%%
%% The default layout of the sink is `logi_sink:default_layout(BaseSink)'.
-spec new(id(), pid()|atom(), logi_sink:sink()) -> logi_sink:sink().
new(Limiter, Destination, BaseSink) ->
    _ = is_atom(Limiter) orelse error(badarg, [Limiter, Destination, BaseSink]),
    _ = is_atom(Destination) orelse is_pid(Destination) orelse error(badarg, [Limiter, Destination, BaseSink]),
    _ = logi_sink:is_sink(BaseSink) orelse error(badarg, [Limiter, Destination, BaseSink]),
    State =
        #?STATE{
            limiter = Limiter,
            sink = BaseSink,
            destination = Destination,
            max_message_queue_len = 10,
            max_bitrate = 1024 * 8,
            period = 1000
           },
    logi_sink:new(?MODULE, State).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Layout, Format, Data, State) ->
    case get_pid(State#?STATE.destination) of
        undefined      -> ok;
        DestinationPid ->
            case is_message_queue_len_overflowed(DestinationPid, State) of
                true  -> notify_omission(message_queue_overflow, Context, State);
                false ->
                    Metadata = logi_context:get_metadata(Context),
                    case maps:get(urgent, Metadata, false) =:= false andalso is_bitrate_exceeded(State) of
                        true  -> notify_omission(bitrate_exceeded, Context, State);
                        false ->
                            IoData = logi_layout:format(Context, Format, Data, Layout),
                            IoDataSize = iolist_size(IoData),
                            ok = notify_write(IoDataSize, State),
                            Sink = State#?STATE.sink,
                            (logi_sink:get_module(Sink)):write(
                              Context, logi_layout_raw:new(), "", IoData, logi_sink:get_extra_data(Sink))
                    end
            end
    end.

%% @private
default_layout(#?STATE{sink = Sink}) ->
    logi_sink:default_layout(Sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec notify_omission(atom(), logi_context:context(), #?STATE{}) -> ok.
notify_omission(Tag, Context, #?STATE{limiter = Limiter}) ->
    logi_sink_flow_limiter_server:notify_omission(Limiter, Tag, Context).

-spec notify_write(non_neg_integer(), #?STATE{}) -> ok.
notify_write(MessageSize, #?STATE{limiter = Limiter}) ->
    logi_sink_flow_limiter_server:notify_write(Limiter, MessageSize).

-spec get_pid(atom() | pid()) -> pid() | undefined.
get_pid(Name) when is_atom(Name) -> whereis(Name);
get_pid(Pid)                     -> Pid.

-spec is_message_queue_len_overflowed(pid(), #?STATE{}) -> boolean().
is_message_queue_len_overflowed(DestinationPid, #?STATE{max_message_queue_len = Max}) ->
    case erlang:process_info(DestinationPid, message_queue_len) of
        {_, Len} when Len < Max -> false;
        _                       -> true
    end.

-spec is_bitrate_exceeded(#?STATE{}) -> boolean().
is_bitrate_exceeded(#?STATE{limiter = Limiter, max_bitrate = Max}) ->
    Bytes = logi_sink_flow_limiter_server:get_write_bytes(Limiter),
    Bits = Bytes * 8,
    Bits > Max.
