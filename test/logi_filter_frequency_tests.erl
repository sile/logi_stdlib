%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_filter_frequency_tests).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, logi_transform}).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(assignMatch(Pattern, Exp),
        begin
            ?assertMatch(Pattern = Pattern, Exp),
            Pattern = Exp
        end).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a new filter instance",
      fun () ->
              Filter = logi_filter_frequency:new(),
              ?assert(logi_filter:is_filter(Filter))
      end},
     {"Creates a filter with options",
      fun () ->
              Filter = logi_filter_frequency:new([{max_count, 1}, {period, 1}]),
              ?assert(logi_filter:is_filter(Filter))
      end},
     {"[ERROR] Invalid argument",
      fun () ->
              ?assertError(badarg, logi_filter_frequency:new(hoge)),             % Not a list
              ?assertError(badarg, logi_filter_frequency:new([{max_count, 0}])), % Invalid integer
              ?assertError(badarg, logi_filter_frequency:new([{period, 0}]))     % Invalid integer
      end}
    ].

filter_test_() ->
    C0 = fun (TimestampMs) ->
                 Timestamp = {TimestampMs div 1000000000, (TimestampMs div 1000) rem 1000000, (TimestampMs rem 1000) * 1000},
                 logi_context:new(test_log, Timestamp, info, logi_location:guess_location(), #{}, #{})
        end,
    C1 = fun (TimestampMs) ->
                 Timestamp = {TimestampMs div 1000000000, (TimestampMs div 1000) rem 1000000, (TimestampMs rem 1000) * 1000},
                 logi_context:new(test_log, Timestamp, info, logi_location:guess_location(), #{}, #{})
        end,
    C2 = fun (TimestampMs) ->
                 logi_context:from_map(maps:merge(logi_context:to_map(C0(TimestampMs)), #{headers => #{id => hoge}}))
         end,
    [
     {"Filters log messages",
      fun () ->
              Filter0 = logi_filter_frequency:new([{max_count, 2}, {period, 1000}]), % 2 messages per second

              %% time: 0 ms (OK)
              ?assignMatch({true, Filter1}, logi_filter:apply(C0(0), Filter0)),

              %% time: 10 ms (OK)
              ?assignMatch({true, Filter2}, logi_filter:apply(C0(10), Filter1)),

              %% time: 20 ms (NG)
              ?assignMatch({false, Filter3}, logi_filter:apply(C0(20), Filter2)),

              %% time: 30 ms (NG)
              ?assignMatch({false, Filter4}, logi_filter:apply(C0(30), Filter3)),

              %% time: 1001 ms (OK)
              ?assignMatch({true, Filter5}, logi_filter:apply(C0(1001), Filter4)),

              %% time: 1002 ms (NG)
              ?assignMatch({false, Filter6}, logi_filter:apply(C0(1002), Filter5)),

              %% time: 5000 ms (OK)
              ?assignMatch({true, Filter7}, logi_filter:apply(C0(5000), Filter6)),

              %% time: 5001 ms (OK)
              ?assignMatch({true, Filter8}, logi_filter:apply(C0(5001), Filter7)),

              %% time: 5002 ms [other location] (OK)
              ?assignMatch({true, Filter9}, logi_filter:apply(C1(5002), Filter8)),

              %% time: 5003 ms [other headers] (OK)
              ?assignMatch({true, Filter10}, logi_filter:apply(C2(5003), Filter9)),

              %% time: 8000 ms (OK)
              ?assignMatch({true, _Filter11}, logi_filter:apply(C0(8000), Filter10))
      end}
    ].
