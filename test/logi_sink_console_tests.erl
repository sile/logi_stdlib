%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_sink_console_tests).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, logi_transform}).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a new instance",
      fun () ->
              Sink = logi_sink_console:new(),
              ?assert(logi_sink:is_spec(Sink))
      end}
    ].

format_test_() ->
    {foreach,
     fun ()     -> {ok, Apps} = application:ensure_all_started(logi_stdlib), Apps end,
     fun (Apps) -> lists:foreach(fun application:stop/1, Apps) end,
     [
      {"Writes log messages",
       fun () ->
               {ok, _} = logi_channel:install_sink(info, logi_sink_console:new()),
               _ = logi:info("hello world"),
               ?assert(true)
       end}
     ]}.
