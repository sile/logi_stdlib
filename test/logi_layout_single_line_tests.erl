%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_layout_single_line_tests).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, logi_transform}).

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a layout instance",
      fun () ->
              Layout = logi_layout_single_line:new(logi_layout_io_lib_format:new()),
              ?assert(logi_layout:is_layout(Layout))
      end},
     {"[ERROR] Invalid argument",
      fun () ->
              ?assertError(badarg, logi_layout_single_line:new(123)) % Not a layout
      end}
    ].

format_test_() ->
    [
     {"Formats a log message",
      fun () ->
              Context = logi_context:new(test_log, os:timestamp(), info, logi_location:guess_location(), #{}, #{}),
              Layout = logi_layout_single_line:new(logi_layout_io_lib_format:new()),
              ?assertEqual(
                 <<"hello world">>,
                 iolist_to_binary(logi_layout:format(Context, "hello \r\nw\n  \t\r\t\t  or\nl\rd\n", [], Layout)))
      end}
    ].
