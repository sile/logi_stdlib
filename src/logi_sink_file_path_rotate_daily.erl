-module(logi_sink_file_path_rotate_daily).

-hebaviour(logi_sink_file_path).

-export([new/1]).

-export([init/1, next_path/1, handle_info/2]).

-define(STATE, ?MODULE).

-record(?STATE,
        {
          pattern :: binary(),
          path  :: binary(),
          today :: term() % calendar:datetime()
        }).

%% {YYYY},{MM},{DD} =>
new(Path) ->
    {Today, _} = calendar:local_time(),
    State =
        #?STATE{
            pattern = list_to_binary(Path),
            path    = make_real_path(list_to_binary(Path), Today),
            today   = Today
           },
    logi_sink_file_path:new(?MODULE, State).

init(State) ->
    Now = calendar:local_time(),
    _ = erlang:send_after(tomorrow(Now), self(), {?MODULE, rotate}),
    State.

make_real_path(Path0, {Year, Month, Day}) ->
    Path1 = re:replace(Path0, "{YYYY}", integer_to_list(Year),  [global, {return, binary}]),
    Path2 = re:replace(Path1, "{MM}",   integer_to_list(Month), [global, {return, binary}]),
    Path3 = re:replace(Path2, "{DD}",   integer_to_list(Day),   [global, {return, binary}]),
    Path3.

next_path(State = #?STATE{path = Path}) ->
    {ok, Path, State}.

handle_info({?MODULE, rotate}, State) ->
    Now = calendar:local_time(),
    case element(1, Now) =:= State#?STATE.today of
        true ->
            _ = erlang:send_after(tomorrow(Now), self(), {?MODULE, rotate}),
            {ok, false, State};
        false ->
            _ = erlang:send_after(tomorrow(Now), self(), {?MODULE, rotate}),
            State1 =
                #?STATE{
                    path = make_real_path(State#?STATE.pattern, element(1, Now)),
                    today = element(1, Now)
                   },
            {ok, true, State1}
    end.

tomorrow({_, Time}) ->
    Current = calendar:time_to_seconds(Time),
    Offset = 1,
    Delta = (24 * 60 * 60 + Offset) - Current,
    Delta * 1000.
