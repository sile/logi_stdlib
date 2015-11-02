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
%% @doc Creates a layout
-spec new() -> logi_layout:layout().
new() ->
    %% TODO: support color option
    logi_layout_limit:new(logi_layout:new(?MODULE)).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec format(logi_context:context(), io:format(), logi_layout:data(), undefined) -> iodata().
format(Context, Format, Data, _) ->
    Location = logi_context:get_location(Context),
    io_lib:format(
      "~s [~s] ~p ~p ~s:~s:~p [~s] " ++ Format ++ "\n",
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
      ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec format_timestamp(erlang:timestamp()) -> iodata().
format_timestamp(Timestamp) ->
    {_, _, Micros} = Timestamp,
    Millis = Micros div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(Timestamp),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~3..0B",
                  [Year, Month, Day, Hour, Minute, Second, Millis]).

-spec format_headers(logi:headers()) -> iodata().
format_headers(Headers) ->
    string:join([[atom_to_list(K),"=",to_string(V)] || {K, V} <- maps:to_list(Headers)], ",").

-spec to_string(term()) -> iodata().
to_string(V) when is_binary(V)    -> binary_to_list(V);
to_string(V) when is_atom(V)      -> atom_to_list(V);
to_string(V) when is_integer(V)   -> integer_to_list(V);
to_string(V) when is_float(V)     -> float_to_list(V);
to_string(V) when is_function(V)  -> erlang:fun_to_list(V);
to_string(V) when is_pid(V)       -> erlang:pid_to_list(V);
to_string(V) when is_port(V)      -> erlang:port_to_list(V);
to_string(V) when is_reference(V) -> erlang:ref_to_list(V);
to_string(V) when is_list(V)      ->
    IsChar = fun (C) -> 0 =< C andalso C =< 16#10ffff end,
    case lists:all(IsChar, V) of
        true  -> V;
        false -> io_lib:format("~1000000p", [V])
    end;
to_string(V) ->
    io_lib:format("~1000000p", [V]).
