%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc "OR" filter
%%
%% === EXAMPLE ===
%% <pre lang="erlang">
%% > Filter =
%%       logi_filter_or:new(
%%         [
%%           %% 'info' or 'alert' messages are allowed
%%           logi_builtin_filter_fun:new(fun (C) -> info  =:= logi_context:get_severity(C) end),
%%           logi_builtin_filter_fun:new(fun (C) -> alert =:= logi_context:get_severity(C) end)
%%         ]).
%% > logi:save_as_default(logi:new([{filter, Filter}])).
%% > logi_channel:install_sink(debug, logi_sink_console:new()).
%% > application:set_env(logi, warn_no_parse_transform, false).
%%
%% > logi:debug("hello world"). % This message is discarded
%%
%% > logi:info("hello world").
%% 2015-11-02 13:24:02.847 [info] nonode@nohost &lt;0.98.0&gt; erl_eval:do_apply:673 [] hello world
%%
%% > logi:notice("hello world"). % This message is discarded
%%
%% > logi:alert("hello world").
%% 2015-11-02 13:25:05.222 [alert] nonode@nohost &lt;0.98.0&gt; erl_eval:do_apply:673 [] hello world
%% </pre>
-module(logi_filter_or).

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
%% The fitler filters messages using the OR boolean operator on the `OrFilters'.
%% If any filter in `OrFilters' returns `true', the entire result will become to `true'.
%%
%% `OrFilters' are applied left to right, and if any filter returns `true', the remaining filters will not be applied.
-spec new([logi_filter:filter()]) -> logi_filter:filter().
new(OrFilters) ->
    _ = is_list(OrFilters) orelse error(badarg, [OrFilters]),
    _ = lists:all(fun logi_filter:is_filter/1, OrFilters) orelse error(badarg, [OrFilters]),
    logi_filter:new(?MODULE, OrFilters).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_filter' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
filter(Context, OrFilters) ->
    Recur =
        fun Recur ([], Acc) ->
                {false, lists:reverse(Acc)};
            Recur ([F0 | Rest], Acc) ->
                case logi_filter:apply(Context, F0) of
                    true        -> {true, lists:reverse(Acc, [F0 | Rest])};
                    {true, F1}  -> {true, lists:reverse(Acc, [F1 | Rest])};
                    false       -> Recur(Rest, [F0 | Acc]);
                    {false, F1} -> Recur(Rest, [F1 | Acc])
                end
        end,
    Recur(OrFilters, []).
