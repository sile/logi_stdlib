%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_sink_file_rotator_daily_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a new instance",
      fun () ->
              Rotator = logi_sink_file_rotator_daily:new(),
              ?assert(logi_sink_file_rotator:is_rotator(Rotator))
      end}
    ].

rotate_test_() ->
    [
     {"The base rotator is logi_sink_file_rotator_noop (default)",
      fun () ->
              FilePath = <<"{YYYY}-{MM}-{DD}.log">>,
              Rotator = logi_sink_file_rotator_daily:new(),
              ?assertEqual({ok, FilePath, Rotator}, logi_sink_file_rotator:rotate(FilePath, Rotator)),

              {Today, _} = calendar:local_time(),
              {Y, M, D} = Today,
              ExpectedPath = iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B.log", [Y, M, D])),
              ?assertEqual({ok, ExpectedPath, Rotator}, logi_sink_file_rotator:get_current_filepath(FilePath, Rotator)),
              ?assertMatch({false, _, Rotator}, logi_sink_file_rotator:is_outdated(FilePath, Rotator))
      end}
    ].
