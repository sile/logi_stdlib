%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%% @end
%%
%% 標準の`error_logger'に出力されたログをlogiに転送するためのソース
%%
%% TODO: 全体的に整理
-module(logi_source_error_logger).

-behaviour(gen_event).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([install/0, install/1]).
-export([uninstall/0]).

-export([default_log_fun/2]).

-export_type([options/0, option/0]).
-export_type([log_fun/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_event' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(STATE, ?MODULE).

-record(?STATE,
        {
          logger                :: logi:logger_instance(),
          forward_logger        :: logi:logger_instance(),
          log_fun               :: log_fun(),
          max_message_queue_len :: non_neg_integer(),
          drop_count = 0        :: non_neg_integer()
        }).

-type log_fun() :: fun ((error_logger_event(), logi:logger_instance()) -> logi:logger_instance()).
%%

%% `error_logger'のログを`logi'に転送するための関数のインタフェース

-type group_leader() :: pid().

-type error_logger_event() :: {error,          group_leader(), {pid(), io:format(), logi_layout:data()}}
                            | {error_report,   group_leader(), {pid(), std_error, Report :: term()}}
                            | {error_report,   group_leader(), {pid(), Type :: term(), Report :: term()}}
                            | {warning_msg,    group_leader(), {pid(), io:format(), logi_layout:data()}}
                            | {warning_report, group_leader(), {pid(), std_warning, Report :: term()}}
                            | {warning_report, group_leader(), {pid(), Type :: term(), Report :: term()}}
                            | {info_msg,       group_leader(), {pid(), io:format(), logi_layout:data()}}
                            | {info_report,    group_leader(), {pid(), std_info, Report :: term()}}
                            | {info_report,    group_leader(), {pid(), Type :: term(), Report :: term()}}.
%%

%% `error_logger'が送信するイベント
%%
%% [error_logger#Events](http://www.erlang.org/doc/man/error_logger.html#id115197)より抜粋

-type options() :: [option()].
-type option() :: {logger, logi:logger()}
                | {forward_logger, logi:logger()}
                | {max_message_queue_len, non_neg_integer()}
                | {log_fun, log_fun()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv install([])
install() -> install([]).

-spec install(options()) -> ok | {error, Reason::term()}.
install(Options) ->
    ForwardLogger = proplists:get_value(forward_logger, Options, logi:default_logger()),
    Logger = proplists:get_value(logger, Options, ForwardLogger),
    MaxLen = proplists:get_value(max_message_queue_len, Options, 128),
    LogFun = proplists:get_value(log_fun, Options, fun ?MODULE:default_log_fun/2),
    _ = logi:is_logger(ForwardLogger) orelse error(badarg, [Options]),
    _ = logi:is_logger(Logger) orelse error(badarg, [Options]),
    _ = is_integer(MaxLen) andalso MaxLen >= 0 orelse error(badarg, [Options]),
    _ = is_function(LogFun, 2) orelse error(badarg, [Options]),

    case error_logger:add_report_handler(?MODULE, [ForwardLogger, Logger, MaxLen, LogFun]) of
        ok              -> ok;
        {error, Reason} -> {error, Reason};
        Other           -> {error, Other}
    end.

-spec uninstall() -> ok | {error, Reason::term()}.
uninstall() ->
    case error_logger:delete_report_handler(?MODULE) of
        ok              -> ok;
        {error, Reason} -> {error, Reason};
        Other           -> {error, Other}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_event' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([ForwardLogger, Logger0, MaxMessageQueueLen, LogFun]) ->
    Logger1 = logi:info("Started: forward_logger=~p, max_message_queue_len=~p, log_fun=~p",
                        [ForwardLogger, MaxMessageQueueLen, LogFun], [{logger, Logger0}]),
    State =
        #?STATE{
            logger = Logger1,
            forward_logger = logi:ensure_to_be_instance(ForwardLogger),
            max_message_queue_len = MaxMessageQueueLen,
            log_fun = LogFun
           },
    {ok, State}.

%% @private
handle_event(Event, State0) ->
    State1 = drop_overflowed_messages(State0),
    case State1#?STATE.drop_count of
        0 ->
            Logger = (State0#?STATE.log_fun)(Event, State1#?STATE.forward_logger),
            State2 = State1#?STATE{forward_logger = Logger},
            {ok, State2};
        C ->
            {ok, State1#?STATE{drop_count = C - 1}}
    end.

%% @private
handle_call(Request, State) ->
    Logger = logi:warning("Unknown call: ~p", [Request], [{logger, State#?STATE.logger}, {metadata, #{log_type => system}}]),
    {ok, {error, {unknown_call, Request}}, State#?STATE{logger = Logger}}.

%% @private
handle_info(Info, State) ->
    Logger = logi:warning("Unknown info: ~p", [Info], [{logger, State#?STATE.logger}, {metadata, #{log_type => system}}]),
    {ok, State#?STATE{logger = Logger}}.

%% @private
terminate(Reason, State) ->
    _ = logi:info("Terminating: reason=~p", [Reason], [{logger, State#?STATE.logger}]),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec drop_overflowed_messages(#?STATE{}) -> #?STATE{}.
drop_overflowed_messages(State = #?STATE{drop_count = Count}) when Count > 0 ->
    State;
drop_overflowed_messages(State) ->
    {message_queue_len, Len} = erlang:process_info(self(), message_queue_len),
    Max = State#?STATE.max_message_queue_len,
    case Len =< Max of
        true  -> State;
        false ->
            Logger =
                logi:warning("The max_message_queue_len is exceeded (~p > ~p). The following ~p events will be discarded.",
                             [Len, Max, Len - Max], [{logger, State#?STATE.logger}, {metadata, #{log_type => system}}]),
            State#?STATE{drop_count = Len - Max, logger = Logger}
    end.

-spec default_log_fun(error_logger_event(), logi:logger_instance()) -> logi:logger_instance().
default_log_fun(Event = {Tag, Gleader, {Sender, Arg1, Arg2}}, Logger) ->
    {LogType, Severity} =
        case Tag of
            error          -> {message, error};
            warning_msg    -> {message, warning};
            info_msg       -> {message, info};
            error_report   -> {report, error};
            warning_report -> {report, warning};
            info_report    -> {report, info};
            _              -> {system, warning}
        end,
    {Format, Data} =
        case LogType of
            message -> {Arg1, Arg2};
            report  -> {"~p", [Arg2]};
            system  -> {"Unknown event: ~p", [Event]}
        end,
    Headers =
        case LogType of
            message -> #{gleader => Gleader, sender => Sender};
            report  -> #{gleader => Gleader, sender => Sender, type => Arg1};
            system  -> #{}
        end,
    MetaData =
        #{log_type => LogType},
    Location = logi_location:guess_location(),
    logi:log(Severity, Format, Data, [{logger, Logger}, {headers, Headers}, {metadata, MetaData}, {location, Location}]);
default_log_fun(Event, Logger) ->
    logi:warning("Unknown event: ~p", [Event], [{logger, Logger}, {metadata, #{log_type => system}}]).
