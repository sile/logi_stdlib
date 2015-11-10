%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi_layout_default).

-behaviour(logi_layout).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([format/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new layout instance
-spec new() -> logi_layout:layout(). % TODO: layout/1
new() -> logi_layout:new(?MODULE).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec format(logi_context:context(), io:format(), logi_layout:data(), undefined) -> iodata().
format(Context, Format, Data, _) ->
    Location = logi_context:get_location(Context),
    FormattedData =
        io_lib:format(
          "~s [~s] ~p ~p ~s:~s:~p [~s] " ++ Format,
          [
           format_timestamp(logi_context:get_timestamp(Context)),
           logi_context:get_severity(Context),
           node(logi_location:get_process(Location)),
           logi_location:get_process(Location),
           logi_location:get_module(Location),
           logi_location:get_function(Location),
           logi_location:get_line(Location),
           format_headers(logi_context:get_headers(Context)) |
           Data
          ]),
    unicode:characters_to_binary(FormattedData). % TODO: optimize

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%% TODO: logi_lib_layout:format_timestamp/1
-spec format_timestamp(erlang:timestamp()) -> iodata().
format_timestamp(Timestamp) ->
    {_, _, Micros} = Timestamp,
    Millis = Micros div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Timestamp),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~3..0B",
                  [Year, Month, Day, Hour, Minute, Second, Millis]).

-spec format_headers(logi:headers()) -> iodata().
format_headers(Headers) ->
    string:join(
      [
       [logi_lib_layout:term_to_iodata(K), "=", logi_lib_layout:term_to_iodata(V)] || {K, V} <- maps:to_list(Headers)
      ], ",").
