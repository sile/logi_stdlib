%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_source_error_logger_tests).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, logi_transform}).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
install_test_() ->
    {foreach,
     fun ()     -> {ok, Apps} = application:ensure_all_started(logi_stdlib), Apps end,
     fun (Apps) -> lists:foreach(fun application:stop/1, Apps) end,
     [
      {"Installs and uninstalls a error_logger handler",
       fun () ->
               ?assertEqual(ok, logi_source_error_logger:install()),
               ?assertEqual(ok, logi_source_error_logger:uninstall())
       end},
      {"Multiple handlers are allowed",
       fun () ->
               ok = logi_source_error_logger:install(),
               ?assertMatch(ok, logi_source_error_logger:install()),
               ?assertEqual(ok, logi_source_error_logger:uninstall()),
               ?assertEqual(ok, logi_source_error_logger:uninstall())
       end}
     ]}.

forward_test_() ->
    {foreach,
     fun ()     ->
             error_logger:tty(false),
             {ok, Apps} = application:ensure_all_started(logi_stdlib), Apps end,
     fun (Apps) ->
             error_logger:tty(true),
             lists:foreach(fun application:stop/1, Apps)
     end,
     [
      {"Forwards log messages",
       fun () ->
               Parent = self(),
               Sink = logi_builtin_sink_fun:new(
                        forward_sink,
                        fun (_, Format, Data) ->
                                Parent ! {msg, lists:flatten(io_lib:format(Format, Data))},
                                []
                        end),
               ok = logi_channel:create(forward_test),
               {ok, _} = logi_channel:install_sink(forward_test, Sink, info),

               ok = logi_source_error_logger:install([{forward_logger, logi:new([{channel, forward_test}])}]),

               error_logger:info_msg("hello"),
               error_logger:info_report(hello),
               error_logger:warning_msg("hello"),
               error_logger:warning_report(hello),
               error_logger:error_msg("hello"),
               error_logger:error_report(hello),
               lists:foreach(
                 fun (_) ->
                         receive
                             {msg, Message} -> ?assertEqual("hello", Message)
                         after 10 -> ?assert(timeout)
                         end
                 end,
                 lists:seq(1, 6))
       end},
      {"Overload protection: max message queue length",
       fun () ->
               Parent = self(),
               Sink = logi_builtin_sink_fun:new(warning_sink, fun (_, Message, _) -> Parent ! {msg, Message}, [] end),
               {ok, _} = logi_channel:install_sink(Sink, warning),

               MaxLength = 0,
               ok = logi_source_error_logger:install([{max_message_queue_len, MaxLength}]),

               lists:foreach(fun (_) -> error_logger:info_msg("hello") end, lists:seq(1, 100)),
               receive
                   {msg, Message} -> ?assertMatch("The max_message_queue_len is exceeded" ++ _, Message)
               after 10 -> ?assert(timeout)
               end

       end}
     ]}.
