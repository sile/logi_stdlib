%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi_filter_and).

-behaviour(logi_filter).

-export([new/1]).

-export([filter/2]).

-spec new([logi_filter:filter()]) -> logi_filter:filter().
new(AndFilters) ->
    logi_filter:new(?MODULE, AndFilters).

filter(_Context, []) ->
    true;
filter(Context, [Filter0 | AndFilters]) ->
    case logi_filter:apply(Context, Filter0) of
        false            -> [Filter0 | AndFilters];
        {false, Filter1} -> [Filter1 | AndFilters];
        true             -> [Filter0 | filter(Context, AndFilters)];
        {true, Filter1}  -> [Filter1 | filter(Context, AndFilters)]
    end.
