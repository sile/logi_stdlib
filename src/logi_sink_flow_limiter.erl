%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ログの出力量を制御するためのシンク
%%
%% このシンクはログ出力の際に、以下の判定を行う:
%% - ログの書き込み先プロセスが生きているか
%% - ログの書き込み先プロセスのメッセージキューが詰まっていないか
%% - ログの出力ペースが指定の範囲内に収まっているか
%%
%% いずれかの条件を満たさなかった場合は、そのログメッセージは破棄される。
%% (破棄されたメッセージが存在する場合は、一定期間毎にまとめて、レポートが出力される)
%%
%% 全ての条件を満たしている場合は、実際のログ出力処理を担っているシンクに後続の処理が委譲される。
%%
%% == EXAMPLE ==
%%
%% TODO
-module(logi_sink_flow_limiter).

-behaviour(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_limiter/1, start_limiter/2]).
-export([stop_limiter/1]).
-export([which_limiters/0]).

-export([new/2]).

-export_type([id/0]).
-export_type([options/0]).
-export_type([destination/0]).

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
          max_bitrate           :: pos_integer()
        }).

-type id() :: atom().
%% The identifier of a limiter

-type options() :: Todo::term().

-type destination() :: pid() | atom().
%% ログメッセージの実際の書き込み先プロセス (or その名前)
%%
%% このプロセスが死活判定やメッセージキュー詰まり判定の対象となる

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv start_limiter(Id, [])
-spec start_limiter(id()) -> {ok, pid()} | {error, Reason::term()}.
start_limiter(Id) ->
    start_limiter(Id, []).

%% @doc Starts a new limiter
-spec start_limiter(id(), options()) -> {ok, pid()} | {error, Reason::term()}.
start_limiter(Id, Options) ->
    %% TODO: validate
    logi_sink_flow_limiter_server_sup:start_child(Id, Options).

%% @doc Stops the limiter
%%
%% If the limiter associated to `Id' does not exists, it is silently ignored.
-spec stop_limiter(id()) -> ok.
stop_limiter(Id) ->
    logi_sink_flow_limiter_server_sup:stop_child(Id).

%% @doc Returns a list of the running limiters
-spec which_limiters() -> [id()].
which_limiters() ->
    [Id || {Id, _} <- logi_sink_flow_limiter_server_sup:which_children()].

%% @doc Creates a new sink instance
%%
%% The default layout of the sink is `logi_sink:default_layout(BaseSink)'.
-spec new(id(), logi_sink:sink()) -> logi_sink:sink().
new(Limiter, BaseSink) ->
    _ = is_atom(Limiter) orelse error(badarg, [Limiter, BaseSink]),
    _ = logi_sink:is_sink(BaseSink) orelse error(badarg, [Limiter, BaseSink]),
    logi_sink:new(?MODULE, {Limiter, BaseSink}).
%% State =
%%         #?STATE{
%%             limiter = Limiter,
%%             sink = BaseSink,
%%             destination = Destination,
%%             max_message_queue_len = 10,
%%             max_bitrate = 1024 * 8
%%            },
%%     logi_sink:new(?MODULE, State).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Layout, Format, Data, {Limiter, BaseSink}) ->
    case logi_sink_flow_limiter_server:get_destination_status(Limiter) of
        dead           -> notify_omission(destination_is_dead, Context, Limiter);
        queue_overflow -> notify_omission(message_queue_overflow, Context, Limiter);
        normal         ->

    case get_pid(State#?STATE.destination) of
        undefined      -> ok; % TODO: notify_omission(destination_does_not_exist)
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
