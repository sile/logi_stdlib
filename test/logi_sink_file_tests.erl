%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_sink_file_tests).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, logi_transform}).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a new instance",
      fun () ->
              Sink = logi_sink_file:new(test, "test.log"),
              ?assert(logi_sink:is_sink(Sink))
      end}
    ].

write_test_() ->
    {foreach,
     fun () ->
             file:delete("test.log"),
             {ok, Apps} = application:ensure_all_started(logi_stdlib),
             Apps
     end,
     fun (Apps) ->
             file:delete("test.log"),
             lists:foreach(fun application:stop/1, Apps)
     end,
     [
      {"Writes log messages",
       fun () ->
               Layout = logi_layout_io_lib_format:new(),
               Sink = logi_sink_file:new(test, "test.log", [{layout, Layout}, {open_opt, [append]}]),
               {ok, _} = logi_channel:install_sink(Sink, info),

               logi:info("hello"),
               timer:sleep(5),
               ?assertEqual({ok, <<"hello">>}, file:read_file("test.log")),

               logi:notice(" world"),
               timer:sleep(5),
               ?assertEqual({ok, <<"hello world">>}, file:read_file("test.log"))
       end}
     ]}.
