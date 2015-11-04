%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi_source_error_logger).

-behaviour(gen_event).

-export([install/0, install/1]).
-export([uninstall/0]).

-export([default_log_fun/2]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE, ?MODULE).

-record(?STATE,
        {
          logger                      :: logi:logger_instance(),
          log_fun                     :: log_fun(),
          max_message_queue_len = 100 :: non_neg_integer(), % TODO: optionize
          drop_count = 0              :: non_neg_integer()
        }).

-type log_fun() :: fun ((error_logger_event(), logi:logger_instance()) -> logi:logger_instance()).
-type error_logger_event() :: term(). % TODO

install() -> install([]).

install(Options) ->
    Logger0 = proplists:get_value(logger, Options, logi:default_logger()),
    _ = logi:is_logger(Logger0) orelse error(badarg, [Options]),
    Logger1 = logi:from_map(logi:to_map(Logger0)), % TODO: logi:load_any/1
    LogFun = proplists:get_value(log_fun, Options, fun ?MODULE:default_log_fun/2),
    _ = is_function(LogFun, 2) orelse error(badarg, [Options]),
    case error_logger:add_report_handler(?MODULE, [Logger1, LogFun]) of
        ok              -> ok;
        {error, Reason} -> {error, Reason};
        Other           -> {error, Other}
    end.

uninstall() ->
    case error_logger:delete_report_handler(?MODULE) of
        ok              -> ok;
        {error, Reason} -> {error, Reason};
        Other           -> {error, Other}
    end.

%% @private
init([Logger, LogFun]) ->
    State = #?STATE{logger = Logger, log_fun = LogFun},
    {ok, State}.

%% @private
handle_event(Event, State0) ->
    State1 = drop_overflowed_messages(State0),
    case State1#?STATE.drop_count of
        0 ->
            Logger = (State0#?STATE.log_fun)(Event, State1#?STATE.logger),
            State2 = State1#?STATE{logger = Logger},
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
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% TODO:
drop_overflowed_messages(State = #?STATE{drop_count = Count}) when Count > 0 ->
    State;
drop_overflowed_messages(State) ->
    {message_queue_len, Len} = erlang:process_info(self(), message_queue_len),
    Max = State#?STATE.max_message_queue_len,
    case Len =< Max of
        true  -> State;
        false ->
            %% TODO: message
            Logger = logi:warning("The max_message_queue_len is exceeded (~p > ~p). The following ~p events will be discarded.",
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
