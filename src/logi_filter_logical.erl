%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc 論理演算を用いて複数のフィルタを結合するためのフィルタ
%%
%% 式は短絡評価される
-module(logi_filter_logical).

-behaviour(logi_filter).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1]).
-export_type([expression/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_filter' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([filter/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type expression() :: {'not', expression()}
                    | {'and', [expression()]}
                    | {'or',  [expression()]}
                    | logi_filter:filter().
%% 論理演算式

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new filter instance
-spec new(expression()) -> logi_filter:filter().
new(Expression) ->
    _ = is_expression(Expression) orelse error(badarg, [Expression]),
    logi_filter:new(?MODULE, Expression).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_filter' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
filter(Context, {'not', E0}) ->
    {Bool, E1} = filter(Context, E0),
    {not Bool, {'not', E1}};
filter(Context, {'and', Es}) ->
    fun F ([], Acc) ->
            {true, {'and', lists:reverse(Acc)}};
        F ([E0 | Rest], Acc) ->
            case filter(Context, E0) of
                {false, E1} -> {false, {'and', lists:reverse(Acc, [E1 | Rest])}};
                {true,  E1} -> F(Rest, [E1 | Acc])
            end
    end(Es, []);
filter(Context, {'or', Es}) ->
    fun F ([], Acc) ->
            {false, {'or', lists:reverse(Acc)}};
        F ([E0 | Rest], Acc) ->
            case filter(Context, E0) of
                {true,  E1} -> {true, {'or', lists:reverse(Acc, [E1 | Rest])}};
                {false, E1} -> F(Rest, [E1 | Acc])
            end
    end(Es, []);
filter(Context, Filter0) ->
    case logi_filter:apply(Context, Filter0) of
        true  -> {true,  Filter0};
        false -> {false, Filter0};
        Other -> Other
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec is_expression(expression() | term()) -> boolean().
is_expression({'not', E})  -> is_expression(E);
is_expression({'and', Es}) -> lists:all(fun is_expression/1, Es);
is_expression({'or',  Es}) -> lists:all(fun is_expression/1, Es);
is_expression(E)           -> logi_filter:is_filter(E).
