%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A handler for the standard error_logger module to forward log messages
%%
%% If {@link install/1} is invoked, then the messages issued via `error_logger' module are forwarded to a logi channel.
%%
%% @end
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
%% A function which forwards log messages to a logi channel

-type group_leader() :: pid().
%% The PID of a group leader.
%%
%% See official document of `error_logger' for more information on "group leader".

-type error_logger_event() :: {error,          group_leader(), {pid(), io:format(), logi_layout:data()}}
                            | {error_report,   group_leader(), {pid(), std_error, Report :: term()}}
                            | {error_report,   group_leader(), {pid(), Type :: term(), Report :: term()}}
                            | {warning_msg,    group_leader(), {pid(), io:format(), logi_layout:data()}}
                            | {warning_report, group_leader(), {pid(), std_warning, Report :: term()}}
                            | {warning_report, group_leader(), {pid(), Type :: term(), Report :: term()}}
                            | {info_msg,       group_leader(), {pid(), io:format(), logi_layout:data()}}
                            | {info_report,    group_leader(), {pid(), std_info, Report :: term()}}
                            | {info_report,    group_leader(), {pid(), Type :: term(), Report :: term()}}.
%% An event which is send by `error_logger'.
%%
%% The list is excerpted from [error_logger#Events](http://www.erlang.org/doc/man/error_logger.html#id115197).

-type options() :: [option()].

-type option() :: {logger, logi:logger()}
                | {forward_logger, logi:logger()}
                | {max_message_queue_len, non_neg_integer()}
                | {log_fun, log_fun()}.
%% logger:
%% - The logger instance which is used to report internal events of the handler
%% - default: `logi:default_logger()'
%%
%% forward_logger:
%% - The logger instance which is used to forward log messages issued via `error_logger'
%% - default: `logi:default_logger()'
%%
%% max_message_queue_len:
%% - Maximum message queue length of the `error_logger' process
%% - While the length exceeds the value, new arrival messages will not be forwarded (i.e., discarded)
%% - default: `128'
%%
%% log_fun:
%% - Log messages forwarding function
%% - default: `fun logi_source_error_logger:default_log_fun/2'

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv install([])
-spec install() -> ok | {error, Reason :: term()}.
install() ->
    install([]).

%% @doc Installs the `error_logger' handler
-spec install(options()) -> ok | {error, Reason :: term()}.
install(Options) ->
    ForwardLogger = proplists:get_value(forward_logger, Options, logi:default_logger()),
    Logger = proplists:get_value(logger, Options, logi:default_logger()),
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

%% @doc Uninstalls the handler
-spec uninstall() -> ok | {error, Reason::term()}.
uninstall() ->
    case error_logger:delete_report_handler(?MODULE) of
        ok              -> ok;
        {error, Reason} -> {error, Reason};
        Other           -> {error, Other}
    end.

%% @doc Default forwarding function
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
    Location = logi_location:guess_location(),
    logi:log(Severity, Format, Data, [{logger, Logger}, {headers, Headers}, {location, Location}]);
default_log_fun(Event, Logger) ->
    logi:warning("Unknown event: ~p", [Event], [{logger, Logger}]).

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
    Logger = logi:warning("Unknown call: ~p", [Request], [{logger, State#?STATE.logger}]),
    {ok, {error, {unknown_call, Request}}, State#?STATE{logger = Logger}}.

%% @private
handle_info(Info, State) ->
    Logger = logi:warning("Unknown info: ~p", [Info], [{logger, State#?STATE.logger}]),
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
            DropCount = Len - Max,
            Logger =
                logi:warning("The max_message_queue_len is exceeded (~p > ~p). The following ~p events will be discarded.",
                             [Len, Max, DropCount], [{logger, State#?STATE.logger}]),
            State#?STATE{drop_count = DropCount, logger = Logger}
    end.
