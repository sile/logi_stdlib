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

-export([install/1, install/2]).
-export([uninstall/0, uninstall/1]).

-export_type([id/0]).
-export_type([options/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(STATE, ?MODULE).

-record(?STATE,
        {
          limiter               :: id(),
          layout                :: logi_layout:layout(),
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

%% @equiv install(Condition, [])
-spec install(logi_sink:condition()) -> logi_channel:install_sink_result().
install(Condition) -> install(Condition, []).

%% @doc Installs a sink
%%
%% The default value of `Options':
%% - id: `logi_sink_flow_limiter'
%% - channel: `logi_channel:default_channel()'
%% - layout: `logi_layout_color:new(logi_builtin_layout_simple:new())' TODO: logi_layout_default
-spec install(logi_sink:condition(), Options) -> logi_channel:install_sink_result() when
      Options :: [Option],
      Option  :: {id, logi_sink:id()}
               | {channel, logi_channel:id()}
               | {layout, logi_layout:layout()}
               | {sink, logi_sink:sink()}
               | {limiter, id()}
               | logi_channel:install_sink_option().
install(Condition, Options) ->
    SinkId = proplists:get_value(id, Options, ?MODULE),
    Channel = proplists:get_value(channel, Options, logi_channel:default_channel()),
    Layout = proplists:get_value(layout, Options, logi_layout_color:new(logi_layout_default:new())), % TODO: delete color
    BaseSink = proplists:get_value(sink, Options),
    Limiter = proplists:get_value(limiter, Options),
    Destination = proplists:get_value(destination, Options),
    _ = logi_layout:is_layout(Layout) orelse error(badarg, [Condition, Options]),
    _ = logi_sink:is_sink(BaseSink) orelse error(badarg, [Condition, Options]),
    _ = is_atom(Limiter) orelse error(badarg, [Condition, Options]),
    _ = is_atom(Destination) orelse is_pid(Destination) orelse error(badarg, [Condition, Options]),

    State =
        #?STATE{
            limiter = Limiter,
            layout = Layout,
            sink = BaseSink,
            destination = Destination,
            max_message_queue_len = 10,
            max_bitrate = 1024 * 8,
            period = 1000
           },
    Sink = logi_sink:new(SinkId, ?MODULE, Condition, State),
    logi_channel:install_sink(Channel, Sink, Options).

%% @equiv uninstall([])
-spec uninstall() -> logi_channel:uninstall_sink_result().
uninstall() -> uninstall([]).

%% @doc Uninstalls a sink
%%
%% The default value of `Options':
%% - id: `logi_sink_flow_limiter'
%% - channel: `logi_channel:default_channel()'
-spec uninstall(Options) -> logi_channel:uninstall_sink_result() when
      Options :: [Option],
      Option  :: {id, logi_sink:id()}
               | {channel, logi_channel:id()}.
uninstall(Options) ->
    SinkId = proplists:get_value(id, Options, ?MODULE),
    Channel = proplists:get_value(channel, Options, logi_channel:default_channel()),
    logi_channel:uninstall_sink(Channel, SinkId).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
write(Context, Format, Data, State) ->
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
                            IoData = logi_layout:format(Context, Format, Data, State#?STATE.layout),
                            IoDataSize = iolist_size(IoData),
                            ok = notify_write(IoDataSize, State),
                            Sink = State#?STATE.sink,
                            (logi_sink:get_module(Sink)):write(Context, IoData, [], logi_sink:get_extra_data(Sink))
                    end
            end
    end.

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
