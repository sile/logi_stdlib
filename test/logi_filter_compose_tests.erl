%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_filter_compose_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creats a new filter instance",
      fun () ->
              Filter0 = logi_filter_compose:new(logi_filter_severity:new(info)),
              ?assert(logi_filter:is_filter(Filter0)),

              Filter1 = logi_filter_compose:new({'and', []}),
              ?assert(logi_filter:is_filter(Filter1)),

              Filter2 = logi_filter_compose:new({'or', []}),
              ?assert(logi_filter:is_filter(Filter2)),

              Filter3 = logi_filter_compose:new({'not', Filter0}),
              ?assert(logi_filter:is_filter(Filter3)),

              Filter4 = logi_filter_compose:new({'and', [Filter0, Filter1, {'or', [{'not', Filter3}, Filter2]}]}),
              ?assert(logi_filter:is_filter(Filter4))
      end},
     {"[ERROR] Invalid argument",
      fun () ->
              ?assertError(badarg, logi_filter_compose:new(123)), % Not a filter instance
              ?assertError(badarg, logi_filter_compose:new({'nand', []})) % Undefined operator
      end}
    ].

filter_test_() ->
    application:set_env(logi, warn_no_parse_transform, false),
    SeverityFilter = fun (Severity) -> logi_filter_severity:new([Severity]) end,
    ChannelFilter = fun (Channel) -> logi_builtin_filter_fun:new(fun (C) -> Channel =:= logi_context:get_channel(C) end) end,
    ErrorFilter = logi_builtin_filter_fun:new(fun (_) -> error(abort) end),
    Apply =
        fun (Channel, Severity, Filter) ->
                case logi_filter:apply(logi_context:new(Channel, Severity), Filter) of
                    {Bool, _} -> Bool;
                    Bool      -> Bool
                end
        end,
    [
     {"NOT Filter",
      fun () ->
              InfoFilter = SeverityFilter(info),

              ?assertEqual(true,  Apply(test_log, info, InfoFilter)),
              ?assertEqual(false, Apply(test_log, info, logi_filter_compose:new({'not', InfoFilter}))),

              ?assertEqual(false, Apply(test_log, alert, InfoFilter)),
              ?assertEqual(true,  Apply(test_log, alert, logi_filter_compose:new({'not', InfoFilter})))
      end},
     {"OR Filter",
      fun () ->
              DebugFilter = SeverityFilter(debug),
              AlertFilter = SeverityFilter(alert),

              ?assertEqual(true,  Apply(test_log, debug,     logi_filter_compose:new({'or', [DebugFilter, AlertFilter]}))),
              ?assertEqual(false, Apply(test_log, info,      logi_filter_compose:new({'or', [DebugFilter, AlertFilter]}))),
              ?assertEqual(true,  Apply(test_log, alert,     logi_filter_compose:new({'or', [DebugFilter, AlertFilter]}))),
              ?assertEqual(false, Apply(test_log, emergency, logi_filter_compose:new({'or', [DebugFilter, AlertFilter]})))
      end},
     {"AND Filter",
      fun () ->
              InfoFilter = SeverityFilter(info),
              HogeFilter = ChannelFilter(hoge_log),

              ?assertEqual(false, Apply(test_log, info,  logi_filter_compose:new({'and', [InfoFilter, HogeFilter]}))),
              ?assertEqual(false, Apply(hoge_log, debug, logi_filter_compose:new({'and', [InfoFilter, HogeFilter]}))),
              ?assertEqual(true,  Apply(hoge_log, info,  logi_filter_compose:new({'and', [InfoFilter, HogeFilter]})))
      end},
     {"Short-Circuit Evaluation",
      fun () ->
              InfoFilter = SeverityFilter(info),

              ?assertEqual(true,  Apply(test_log, info, logi_filter_compose:new({'or', [InfoFilter, ErrorFilter]}))),
              ?assertError(abort, Apply(test_log, debug, logi_filter_compose:new({'or', [InfoFilter, ErrorFilter]}))),
              ?assertEqual(false, Apply(test_log, debug, logi_filter_compose:new({'and', [InfoFilter, ErrorFilter]}))),
              ?assertError(abort, Apply(test_log, info,  logi_filter_compose:new({'and', [InfoFilter, ErrorFilter]})))
      end},
     {"Compound Filter",
      fun () ->
              InfoFilter = SeverityFilter(info),
              AlertFilter = SeverityFilter(alert),
              ?assertEqual(true, Apply(test_log, info, logi_filter_compose:new({'and', [{'not', AlertFilter}, {'or', [AlertFilter, InfoFilter]}]})))
      end}
    ].
