%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_sink_file_rotator_noop_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a new instance",
      fun () ->
              Rotator = logi_sink_file_rotator_noop:new(),
              ?assert(logi_sink_file_rotator:is_rotator(Rotator))
      end}
    ].

rotate_test_() ->
    [
     {"Does not rotate file",
      fun () ->
              FilePath = <<"test.log">>,
              Rotator = logi_sink_file_rotator_noop:new(),
              ?assertEqual({ok, FilePath, Rotator}, logi_sink_file_rotator:rotate(FilePath, Rotator)),
              ?assertEqual({ok, FilePath, Rotator}, logi_sink_file_rotator:get_current_filepath(FilePath, Rotator)),
              ?assertEqual({false, infinity, Rotator}, logi_sink_file_rotator:is_outdated(FilePath, Rotator))
      end}
    ].
