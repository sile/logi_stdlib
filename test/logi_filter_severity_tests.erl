%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_filter_severity_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creats a new filter instance",
      fun () ->
              Filter0 = logi_filter_severity:new(info),
              ?assert(logi_filter:is_filter(Filter0)),

              Filter1 = logi_filter_severity:new({info, alert}),
              ?assert(logi_filter:is_filter(Filter1)),

              Filter2 = logi_filter_severity:new([debug, info, critical]),
              ?assert(logi_filter:is_filter(Filter2))
      end},
     {"[ERROR] Invalid argument",
      fun () ->
              ?assertError(badarg, logi_filter_severity:new(fatal)) % Undefined severity
      end}
    ].

filter_test_() ->
    application:set_env(logi, warn_no_parse_transform, false),
    Context = fun (Severity) -> logi_context:new(test_log, Severity) end,
    [
     {"Level filter",
      fun () ->
              Filter = logi_filter_severity:new(notice),
              ?assertNot(logi_filter:apply(Context(debug), Filter)),
              ?assertNot(logi_filter:apply(Context(info), Filter)),
              ?assert(logi_filter:apply(Context(notice), Filter)),
              ?assert(logi_filter:apply(Context(warning), Filter)),
              ?assert(logi_filter:apply(Context(error), Filter)),
              ?assert(logi_filter:apply(Context(critical), Filter)),
              ?assert(logi_filter:apply(Context(alert), Filter)),
              ?assert(logi_filter:apply(Context(emergency), Filter))
      end},
     {"Range filter",
      fun () ->
              Filter = logi_filter_severity:new({info, error}),
              ?assertNot(logi_filter:apply(Context(debug), Filter)),
              ?assert(logi_filter:apply(Context(info), Filter)),
              ?assert(logi_filter:apply(Context(notice), Filter)),
              ?assert(logi_filter:apply(Context(warning), Filter)),
              ?assert(logi_filter:apply(Context(error), Filter)),
              ?assertNot(logi_filter:apply(Context(critical), Filter)),
              ?assertNot(logi_filter:apply(Context(alert), Filter)),
              ?assertNot(logi_filter:apply(Context(emergency), Filter))
      end},
     {"List filter",
      fun () ->
              Filter = logi_filter_severity:new([info, error, alert]),
              ?assertNot(logi_filter:apply(Context(debug), Filter)),
              ?assert(logi_filter:apply(Context(info), Filter)),
              ?assertNot(logi_filter:apply(Context(notice), Filter)),
              ?assertNot(logi_filter:apply(Context(warning), Filter)),
              ?assert(logi_filter:apply(Context(error), Filter)),
              ?assertNot(logi_filter:apply(Context(critical), Filter)),
              ?assert(logi_filter:apply(Context(alert), Filter)),
              ?assertNot(logi_filter:apply(Context(emergency), Filter))
      end}
    ].
