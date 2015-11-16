%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc 深刻度に応じたフィルタリングを行うモジュール
-module(logi_filter_severity).

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
-spec new(logi_condition:severity_condition()) -> logi_filter:filter().
new(SeverityCondition) ->
    _ = not is_map(SeverityCondition) andalso logi_condition:is_condition(SeverityCondition) orelse error(badarg, [SeverityCondition]),
    AllowedSeverities = gb_sets:from_list(logi_condition:normalize(SeverityCondition)),
    logi_filter:new(?MODULE, AllowedSeverities).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_filter' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
filter(Context, AllowedSeverities) ->
    gb_sets:is_member(logi_context:get_severity(Context), AllowedSeverities).
