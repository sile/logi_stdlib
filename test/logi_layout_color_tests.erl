%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_layout_color_tests).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, logi_transform}).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a new layout instance",
      fun () ->
              Layout0 = logi_layout_color:new(logi_layout_raw:new()),
              ?assert(logi_layout:is_layout(Layout0)),

              Layout1 = logi_layout_color:new(logi_layout_raw:new(), fun (_) -> "\e[0m" end),
              ?assert(logi_layout:is_layout(Layout1))
      end},
     {"[ERROR] Invalid argument",
      fun () ->
              ?assertError(badarg, logi_layout_color:new(123)), % Not a layout
              ?assertError(badarg, logi_layout_color:new(logi_layout_raw:new(), fun (_, _) -> "\e[0m" end)) % Unexpected function
      end}
    ].

format_test_() ->
    C = fun (Severity) ->
                logi_context:new(test_log, os:timestamp(), Severity, logi_location:guess_location(), #{}, #{})
        end,
    [
     {"Formats log messages",
      fun () ->
              Layout = logi_layout_color:new(logi_layout_raw:new()),
              ?assertEqual(<<"\e[0mhello\e[0m">>, iolist_to_binary(logi_layout:format(C(debug), "", ["hello"], Layout))),
              ?assertEqual(<<"\e[1mhello\e[0m">>, iolist_to_binary(logi_layout:format(C(info), "", ["hello"], Layout))),
              ?assertEqual(<<"\e[1;35mhello\e[0m">>, iolist_to_binary(logi_layout:format(C(notice), "", ["hello"], Layout))),
              ?assertEqual(<<"\e[1;33mhello\e[0m">>, iolist_to_binary(logi_layout:format(C(warning), "", ["hello"], Layout))),
              ?assertEqual(<<"\e[1;31mhello\e[0m">>, iolist_to_binary(logi_layout:format(C(error), "", ["hello"], Layout))),
              ?assertEqual(<<"\e[1;31mhello\e[0m">>, iolist_to_binary(logi_layout:format(C(critical), "", ["hello"], Layout))),
              ?assertEqual(<<"\e[1;7;31mhello\e[0m">>, iolist_to_binary(logi_layout:format(C(alert), "", ["hello"], Layout))),
              ?assertEqual(<<"\e[1;7;31mhello\e[0m">>, iolist_to_binary(logi_layout:format(C(emergency), "", ["hello"], Layout)))
      end},
     {"Custom color",
      fun () ->
              Color = fun (_) -> "\e[1m" end,
              Layout = logi_layout_color:new(logi_layout_raw:new(), Color),
              ?assertEqual(<<"\e[1mhello\e[0m">>, iolist_to_binary(logi_layout:format(C(debug), "", ["hello"], Layout))),
              ?assertEqual(<<"\e[1mhello\e[0m">>, iolist_to_binary(logi_layout:format(C(info), "", ["hello"], Layout))),
              ?assertEqual(<<"\e[1mhello\e[0m">>, iolist_to_binary(logi_layout:format(C(notice), "", ["hello"], Layout)))
      end}
    ].
