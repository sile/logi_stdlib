%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_layout_newline_tests).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, logi_transform}).

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a layout instance",
      fun () ->
              Layout0 = logi_layout_newline:new(logi_layout_raw:new()),
              ?assert(logi_layout:is_layout(Layout0)),

              Layout1 = logi_layout_newline:new(logi_layout_raw:new(), lf),
              ?assert(logi_layout:is_layout(Layout1)),

              Layout2 = logi_layout_newline:new(logi_layout_raw:new(), cr),
              ?assert(logi_layout:is_layout(Layout2)),

              Layout3 = logi_layout_newline:new(logi_layout_raw:new(), crlf),
              ?assert(logi_layout:is_layout(Layout3))
      end},
     {"[ERROR] Invalid argument",
      fun () ->
              ?assertError(badarg, logi_layout_newline:new(123)), % Not a layout
              ?assertError(badarg, logi_layout_newline:new(logi_layout_raw:new(), hoge)) % Undefined style
      end}
    ].

format_test_() ->
    [
     {"Formats log messages",
      fun () ->
              Context = logi_context:new(test_log, os:timestamp(), info, logi_location:guess_location(), #{}, #{}),
              Base = logi_layout_raw:new(),
              ?assertEqual(<<"hello\n">>, iolist_to_binary(logi_layout:format(Context, "", ["hello"], logi_layout_newline:new(Base)))),
              ?assertEqual(<<"hello\n">>, iolist_to_binary(logi_layout:format(Context, "", ["hello"], logi_layout_newline:new(Base, lf)))),
              ?assertEqual(<<"hello\r">>, iolist_to_binary(logi_layout:format(Context, "", ["hello"], logi_layout_newline:new(Base, cr)))),
              ?assertEqual(<<"hello\r\n">>, iolist_to_binary(logi_layout:format(Context, "", ["hello"], logi_layout_newline:new(Base, crlf))))
      end}
    ].
