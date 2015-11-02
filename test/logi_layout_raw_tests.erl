%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_layout_raw_tests).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, logi_transform}).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a new layout instance",
      fun () ->
              Layout = logi_layout_raw:new(),
              ?assert(logi_layout:is_layout(Layout))
      end}
    ].

format_test_() ->
    [
     {"Formats a log message",
      fun () ->
              Context = logi_context:new(test_log, os:timestamp(), info, logi_location:guess_location(), #{}, #{}),
              Layout = logi_layout_raw:new(),
              Data = ["hoge"],
              ?assertEqual(Data, logi_layout:format(Context, "format: ~p", Data, Layout))
      end}
    ].
