%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc "AND" filter
%%
%% === EXAMPLE ===
%% <pre lang="erlang">
%% > Filter =
%%       logi_filter_and:new(
%%         [
%%           logi_builtin_filter_fun:new(fun (C) -> info  =:= logi_context:get_severity(C) end),
%%           logi_builtin_filter_fun:new(fun (C) -> maps:is_key(id, logi_context:get_headers(C)) end)
%%         ]).
%% > logi:save_as_default(logi:new([{filter, Filter}])).
%% > logi_channel:install_sink(debug, logi_sink_console:new()).
%% > application:set_env(logi, warn_no_parse_transform, false).
%%
%% > logi:info("hello world"). % This message is discarded
%%
%% > logi:debug("hello world", [], [{headers, #{id => hoge}}]). % This message is discarded
%%
%% > logi:info("hello world", [], [{headers, #{id => hoge}}]).
%% 2015-11-02 13:46:26.272 [info] nonode@nohost &lt;0.98.0&gt; erl_eval:do_apply:673 [id=hoge] hello world
%% </pre>
-module(logi_filter_and).

-behaviour(logi_filter).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_filter' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([filter/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new filter instance
%%
%% The fitler filters messages using the AND boolean operator on the `AndFilters'.
%% If any filter in `AndFilters' returns `false', the entire result will become to `false'.
%%
%% `AndFilters' are applied left to right, and if any filter returns `false', the remaining filters will not be applied.
-spec new([logi_filter:filter()]) -> logi_filter:filter().
new(AndFilters) ->
    _ = is_list(AndFilters) orelse error(badarg, [AndFilters]),
    _ = lists:all(fun logi_filter:is_filter/1, AndFilters) orelse error(badarg, [AndFilters]),
    logi_filter:new(?MODULE, AndFilters).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_filter' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
filter(Context, AndFilters) ->
    Recur =
        fun Recur ([], Acc) ->
                {true, lists:reverse(Acc)};
            Recur ([F0 | Rest], Acc) ->
                case logi_filter:apply(Context, F0) of
                    true        -> Recur(Rest, [F0 | Acc]);
                    {true, F1}  -> Recur(Rest, [F1 | Acc]);
                    false       -> {false, lists:reverse(Acc, [F0 | Rest])};
                    {false, F1} -> {false, lists:reverse(Acc, [F1 | Rest])}
                end
        end,
    Recur(AndFilters, []).
