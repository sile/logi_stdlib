%% @copyright 2014-2015 Takeru Ohta <phjgt308@gmail.com>
-module(logi_layout_limit_tests).

-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, logi_transform}).

%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a layout instance",
      fun () ->
              Layout0 = logi_layout_limit:new(logi_layout_raw:new()),
              ?assert(logi_layout:is_layout(Layout0)),

              Layout1 = logi_layout_limit:new(logi_layout_raw:new(), [{max_width, 1}, {max_depth, 1}, {max_size, 1}]),
              ?assert(logi_layout:is_layout(Layout1)),

              Layout2 = logi_layout_limit:new(logi_layout_raw:new(),
                                              [{max_width, infinity}, {max_depth, infinity}, {max_size, infinity}]),
              ?assert(logi_layout:is_layout(Layout2))
      end},
     {"[ERROR] Invalid argument",
      fun () ->
              ?assertError(badarg, logi_layout_limit:new(123)), % Not a layout
              ?assertError(badarg, logi_layout_limit:new(logi_layout_raw:new(), [{max_width, 0}])),
              ?assertError(badarg, logi_layout_limit:new(logi_layout_raw:new(), [{max_depth, 0}])),
              ?assertError(badarg, logi_layout_limit:new(logi_layout_raw:new(), [{max_size, 0}]))
      end}
    ].

format_test_() ->
    [
     {"Formats log messages",
      fun () ->
              Context = logi_context:new(test_log, os:timestamp(), info, logi_location:guess_location(), #{}, #{}),
              BaseLayout = logi_builtin_layout_fun:new(fun (_, Format, Data) -> io_lib:format(Format, Data) end),
              Layout = logi_layout_limit:new(BaseLayout, [{max_width, 5}, {max_depth, 2}, {max_size, 60}]),

              %% OK
              ?assertEqual(
                 <<"short">>,
                 iolist_to_binary(logi_layout:format(Context, "~s", ["short"], Layout))),
              ?assertEqual(
                 <<"{0,1,2,3,4}">>,
                 iolist_to_binary(logi_layout:format(Context, "~p", [{0,1,2,3,4}], Layout))),

              %% Width Limit: list
              ?assertEqual(
                 <<"too l...">>,
                 iolist_to_binary(logi_layout:format(Context, "~s", ["too long: hoge hoge"], Layout))),

              ?assertEqual(
                 <<"[a,b,c,d,e,'...']">>,
                 iolist_to_binary(logi_layout:format(Context, "~p", [[a,b,c,d,e,f,g]], Layout))),

              %% Width Limit: binary
              ?assertEqual(
                 <<"too l...">>,
                 iolist_to_binary(logi_layout:format(Context, "~s", [<<"too long: hoge hoge">>], Layout))),

              %% Width Limit: tuple
              ?assertEqual(
                 <<"{0,1,2,3,4,'...'}">>,
                 iolist_to_binary(logi_layout:format(Context, "~p", [{0,1,2,3,4,5,6,7,8,9,10,11}], Layout))),

              %% Width Limit: map
              ?assertEqual(
                 <<"#{0 => 0,1 => 1,2 => 2,3 => 3,4 => 4,'...' => '...'}">>,
                 iolist_to_binary(logi_layout:format(Context, "~p", [maps:from_list([{X,X} || X <- lists:seq(0,11)])], Layout))),

              %% Depth Limit: list
              ?assertEqual(
                 <<"[1,[2,'...']]">>,
                 iolist_to_binary(logi_layout:format(Context, "~p", [[1, [2, [3, [4]]]]], Layout))),

              %% Depth Limit: tuple
              ?assertEqual(
                 <<"{1,{2,'...'}}">>,
                 iolist_to_binary(logi_layout:format(Context, "~p", [{1, {2, {3, {4}}}}], Layout))),

              %% Depth Limit: map
              ?assertEqual(
                 <<"#{1 => #{2 => '...'}}">>,
                 iolist_to_binary(logi_layout:format(Context, "~p", [#{1 => #{2 => #{3 => #{4 => 5}}}}], Layout))),

              %% Depth Limit: list + tuple + map
              ?assertEqual(
                 <<"#{1 => [2,'...'],a => {b,'...',<<\"d\">>}}">>,
                 iolist_to_binary(logi_layout:format(Context, "~p", [#{1 => [2, {3}], a => {b,[c],<<"d">>}}], Layout))),

              %% Size Limit
              ?assertEqual(
                 <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa...10...">>,
                 iolist_to_binary(logi_layout:format(Context, "~s", [list_to_atom(lists:duplicate(70, $a))], Layout)))
      end}
    ].
