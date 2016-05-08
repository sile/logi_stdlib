%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Utility functions for logi_layout
%% @end
-module(logi_lib_layout).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([term_to_iodata/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Converts a term to a readable string(iodata)
-spec term_to_iodata(V :: term()) -> iodata().
term_to_iodata(V) when is_binary(V)    -> V;
term_to_iodata(V) when is_atom(V)      -> atom_to_list(V);
term_to_iodata(V) when is_integer(V)   -> integer_to_list(V);
term_to_iodata(V) when is_float(V)     -> float_to_list(V);
term_to_iodata(V) when is_function(V)  -> erlang:fun_to_list(V);
term_to_iodata(V) when is_pid(V)       -> erlang:pid_to_list(V);
term_to_iodata(V) when is_port(V)      -> erlang:port_to_list(V);
term_to_iodata(V) when is_reference(V) -> erlang:ref_to_list(V);
term_to_iodata(V) when is_list(V)      ->
    IsChar = fun (C) -> is_integer(C) andalso 0 =< C andalso C =< 16#10ffff end,
    case lists:all(IsChar, V) of
        true  -> V;
        false -> io_lib:format("~1000000p", [V])
    end;
term_to_iodata(V) ->
    io_lib:format("~1000000p", [V]).
