-module(logi_filter_frequency).

-behaviour(logi_filter).

-export([new/0, new/1]).

-export([filter/2]).

-define(IS_NON_NEG_INT(X), (is_integer(X) andalso X >= 0)).
-define(STATE, ?MODULE).

-record(frequency_spec,
        {
          intensity = 5   :: non_neg_integer(),
          period  = 60000 :: timeout()
        }).

-record(normal,
        {
          write_count = 0 :: non_neg_integer()
        }).

-record(overflow,
        {
          drop_count = 1 :: non_neg_integer(),
          context        :: logi_context:context()
        }).

-record(?STATE,
        {
          id_to_status = #{}             :: #{location_id() => status()},
          expires = logi_util_heap:new() :: logi_util_heap:heap(expire_entry()),
          frequency_spec                 :: #frequency_spec{}
        }).

-type status() :: #normal{} | #overflow{}.
-type expire_entry() :: term(). % TODO:
-type location_id() :: term(). % TODO:
-type non_neg_milliseconds() :: non_neg_integer().

%% @equiv new([])
-spec new() -> logi_filter:filter().
new() -> new([]).

-spec new(Options) -> logi_filter:filter() when
      Options :: [Option],
      Option  :: {intensity, non_neg_integer()}
               | {period, timeout()}.
new(Options) ->
    _ = is_list(Options) orelse error(badarg, [Options]),
    Intensity = proplists:get_value(intensity, Options, 5),
    Period = proplists:get_value(period, Options, 60000),
    _ = ?IS_NON_NEG_INT(Intensity) orelse error(badarg, [Options]),
    _ = ?IS_NON_NEG_INT(Intensity) orelse Intensity =:= infinity orelse error(badarg, [Options]),

    Spec =
        #frequency_spec{
           intensity = Intensity,
           period    = Period
          },
    State = #?STATE{frequency_spec = Spec},
    logi_filter:new(?MODULE, State).

filter(Context, State0) ->
    State1 = flush_expired_entries(logi_context:get_timestamp(Context), State0),
    LocationId = get_location_id(Context),
    #?STATE{id_to_status = IdToStatus0, expires = Expires0, frequency_spec = #frequency_spec{intensity = Intensity, period = Period}} = State1,
    Status0 = maps:get(LocationId, IdToStatus0, #normal{}),
    case Status0 of
        #overflow{drop_count = Drops} ->
            IdToStatus1 = maps:put(LocationId, Status0#overflow{drop_count = Drops + 1}, IdToStatus0),
            {false, State1#?STATE{id_to_status = IdToStatus1}};
        #normal{write_count = Writes} when Writes >= Intensity ->
            Status1 = #overflow{context = Context},
            IdToStatus1 = maps:put(LocationId, Status1, IdToStatus0),
            {false, State1#?STATE{id_to_status = IdToStatus1}};
        #normal{write_count = Writes} ->
            Status1 = Status0#normal{write_count = Writes + 1},
            IdToStatus1 = maps:put(LocationId, Status1, IdToStatus0),
            Expires1 =
                case Period of
                    infinity -> Expires0;
                    _        ->
                        ExpiryTime = to_milliseconds(logi_context:get_timestamp(Context)) + Period,
                        logi_util_heap:in({ExpiryTime, LocationId}, Expires0)
                end,
            {true, State1#?STATE{id_to_status = IdToStatus1, expires = Expires1}}
    end.

-spec get_location_id(logi_context:context()) -> location_id().
get_location_id(Context) ->
    Location = logi_context:get_location(Context),
    {
      logi_location:get_module(Location),
      logi_location:get_line(Location),
      logi_context:get_headers(Context)
    }.

-spec to_milliseconds(erlang:timestamp()) -> non_neg_integer().
to_milliseconds({Mega, Seconds, Micro}) ->
    (Mega * 1000 * 1000 * 1000) + (Seconds * 1000) + (Micro div 1000).

-spec flush_expired_entries(erlang:timestamp(), #?STATE{}) -> #?STATE{}.
flush_expired_entries(Now, State) ->
    #?STATE{id_to_status = IdToStatus0, expires = Expires0} = State,
    case logi_util_heap:is_empty(Expires0) of
        true  -> State;
        false ->
            {IdToStatus1, Expires1} = flush_expired_entries(to_milliseconds(Now), IdToStatus0, Expires0, State),
            State#?STATE{id_to_status = IdToStatus1, expires = Expires1}
    end.

%%-spec flush_expired_entries(non_neg_integer(),
flush_expired_entries(Now, IdToStatus0, Expires0, State) ->
    case logi_util_heap:peek(Expires0) of
        empty                                 -> {IdToStatus0, Expires0};
        {ExpiryTime, _} when ExpiryTime > Now -> {IdToStatus0, Expires0};
        {_, LocationId}                       ->
            Status = maps:get(LocationId, IdToStatus0),
            Expires1 = logi_util_heap:out(Expires0),
            IdToStatus1 =
                case Status of
                    #normal{write_count = 0} -> maps:remove(LocationId, IdToStatus0);
                    #normal{write_count = C} -> maps:update(LocationId, #normal{write_count = C - 1}, IdToStatus0);
                    #overflow{}              ->
                        ok = output_overflow_message(Now, Status),
                        Intensity = State#?STATE.frequency_spec#frequency_spec.intensity,
                        maps:update(LocationId, #normal{write_count = Intensity - 1} ,IdToStatus0)
                end,
            flush_expired_entries(Now, IdToStatus1, Expires1, State)
    end.

-spec output_overflow_message(non_neg_milliseconds(), #overflow{}) -> ok.
output_overflow_message(Now, #overflow{drop_count = Drops, context = Context}) ->
    Duration = (Now - to_milliseconds(logi_context:get_timestamp(Context))) / 1000,
    Severity = logi_context:get_severity(Context),
    _ = logi:Severity("Over a period of ~p seconds, ~p messages were dropped", [Duration, Drops],
                      [
                       {logger,   logi:new([{channel, logi_context:get_channel(Context)}])},
                       {location, logi_context:get_location(Context)},
                       {headers,  logi_context:get_headers(Context)},
                       {metadata, logi_context:get_metadata(Context)}
                      ]),
    ok.
