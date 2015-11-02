%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_filter_or_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creats a new filter instance",
      fun () ->
              Filter0 = logi_filter_or:new([]),
              ?assert(logi_filter:is_filter(Filter0)),

              Filter1 = logi_filter_or:new([Filter0, logi_builtin_filter_fun:new(fun (_) -> true end)]),
              ?assert(logi_filter:is_filter(Filter1))
      end},
     {"[ERROR] Invalid argument",
      fun () ->
              Filter = logi_builtin_filter_fun:new(fun (_) -> true end),
              ?assertError(badarg, logi_filter_or:new(Filter)),       % Not a list
              ?assertError(badarg, logi_filter_or:new([Filter, 123])) % Not a filter value is included
      end}
    ].

filter_test_() ->
    application:set_env(logi, warn_no_parse_transform, false),
    [
     {"Empty OR filters (every messages are discarded)",
      fun () ->
              Filter = logi_filter_or:new([]),
              ?assertMatch({false, _}, logi_filter:apply(logi_context:new(test_log, debug), Filter))
      end},
     {"OR Filters",
      fun () ->
              Filter =
                  logi_filter_or:new(
                    [
                     logi_builtin_filter_fun:new(fun (C) -> info  =:= logi_context:get_severity(C) end),
                     logi_builtin_filter_fun:new(fun (C) -> alert =:= logi_context:get_severity(C) end)
                    ]),
              ?assertMatch({false, _}, logi_filter:apply(logi_context:new(test_log, debug),  Filter)),
              ?assertMatch({true,  _}, logi_filter:apply(logi_context:new(test_log, info),   Filter)),
              ?assertMatch({false, _}, logi_filter:apply(logi_context:new(test_log, notice), Filter)),
              ?assertMatch({true,  _}, logi_filter:apply(logi_context:new(test_log, alert),  Filter))
      end},
     {"Short-Circuit Evaluation",
      fun () ->
              Filter =
                  logi_filter_or:new(
                    [
                     logi_builtin_filter_fun:new(fun (C) -> info  =:= logi_context:get_severity(C) end),
                     logi_builtin_filter_fun:new(fun (_) -> error(something_wrong) end)
                    ]),
              ?assertError(something_wrong, logi_filter:apply(logi_context:new(test_log, debug), Filter)),
              ?assertMatch({true,  _}, logi_filter:apply(logi_context:new(test_log, info), Filter))
      end}
    ].
