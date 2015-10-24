%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
-module(logi_filter_or).

-behaviour(logi_filter).

-export([new/1]).

-export([filter/2]).

-spec new([logi_filter:filter()]) -> logi_filter:filter().
new(OrFilters) ->
    logi_filter:new(?MODULE, OrFilters).

filter(_Context, []) ->
    false;
filter(Context, [Filter0 | OrFilters]) ->
    case logi_filter:apply(Context, Filter0) of
        true             -> [Filter0 | OrFilters];
        {true, Filter1}  -> [Filter1 | OrFilters];
        false            -> [Filter0 | filter(Context, OrFilters)];
        {false, Filter1} -> [Filter1 | filter(Context, OrFilters)]
    end.
