%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% NOTE: headers is not limited
%%
%% TODO: reduce overhead (check => adjust if need)
%%
%% TODO: logi_builtin_layout_simple => Check non unicode integer (to_string/1)
%%
%% TODO: logi_layout_singlelinefy, logi_layout_pp (stacktrace, record, etc)
%%
%% @doc TODO
-module(logi_layout_limit).

-behaviour(logi_layout).

-export([new/1, new/2]).
-export([format/4]).

-record(extra,
        {
          layout               :: logi_layout:layout(),
          max_line_length = 80 :: pos_integer() | infinity,
          max_width = 1024     :: pos_integer() | infinity,
          max_depth = 16       :: pos_integer() | infinity,
          max_size  = 10240    :: pos_integer() | infinity
        }).

-define(MAX_LINE_LENGTH_OTP_DEFAULT, 80).
-define(INFINITY_SIZE, (1024 * 1024 * 1024)). % 1GB

new(BaseLayout) ->
    new(BaseLayout, []).

new(BaseLayout, Options) ->
    %% TODO: validate
    MaxLineLength = proplists:get_value(max_line_length, Options, infinity),
    MaxWidth = proplists:get_value(max_width, Options, 1024),
    MaxDepth = proplists:get_value(max_depth, Options, 16),
    MaxSize = proplists:get_value(max_size, Options, 10240),
    Extra =
        #extra{
           layout          = BaseLayout,
           max_line_length = MaxLineLength,
           max_width       = MaxWidth,
           max_depth       = MaxDepth,
           max_size        = MaxSize
          },
    logi_layout:new(?MODULE, Extra).

format(Context, Format0, Data0, Extra) ->
    Format1 =
        case Extra#extra.max_line_length of
            ?MAX_LINE_LENGTH_OTP_DEFAULT -> Format0;
            infinity                     -> adjust_format(Format0, ?INFINITY_SIZE);
            MaxLineLength                -> adjust_format(Format0, MaxLineLength)
        end,
    Data1 =
        [adjust_data(X, 0, Extra#extra.max_depth, Extra#extra.max_width) || X <- Data0],
    Result = logi_layout:format(Context, Format1, Data1, Extra#extra.layout),
    case iolist_size(Result) =< Extra#extra.max_size of
        true  -> Result;
        false -> abbreviate(Result, Extra#extra.max_size)
    end.

adjust_format(Format, MaxLen) when is_atom(Format) ->
    adjust_format(atom_to_list(Format), MaxLen);
adjust_format(Format, MaxLen) when is_binary(Format) ->
    adjust_format(binary_to_list(Format), MaxLen);
adjust_format(Format, MaxLen) ->
    adjust_format_string(Format, integer_to_list(MaxLen)).

adjust_format_string([], _) ->
    [];
adjust_format_string("~p" ++ Format, MaxLen) ->
    "~" ++ MaxLen ++ "p" ++ adjust_format_string(Format, MaxLen);
adjust_format_string("P" ++ Format, MaxLen) ->
    "~" ++ MaxLen ++ "P" ++ adjust_format_string(Format, MaxLen);
adjust_format_string("tp" ++ Format, MaxLen) ->
    "~" ++ MaxLen ++ "tp" ++ adjust_format_string(Format, MaxLen);
adjust_format_string("tP" ++ Format, MaxLen) ->
    "~" ++ MaxLen ++ "tP" ++ adjust_format_string(Format, MaxLen);
adjust_format_string([$~, C | Format], MaxLen) ->
    [$~, C, adjust_format_string(Format, MaxLen)];
adjust_format_string([C | Format], MaxLen) ->
    [C | adjust_format_string(Format, MaxLen)].

adjust_data(_, Depth, Depth, _) ->
    '...';
adjust_data(List, Depth, MaxDepth, MaxWidth) when is_list(List) ->
    try
        {_, Result} =
            lists:foldl(
              fun (_, {I, Acc}) when I > MaxWidth ->
                      {I, Acc};
                  (_, {I, Acc}) when I =:= MaxWidth ->
                      throw({break, lists:reverse(['...' | Acc])});
                  (X, {I, Acc}) ->
                      {I + 1, [adjust_data(X, Depth + 1, MaxDepth, MaxWidth) | Acc]}
              end,
              {0, []},
              List),
        lists:reverse(Result)
    catch
        throw:{break, ResultList} ->
            ResultList
    end;
adjust_data(Tuple, Depth, MaxDepth, MaxWidth) when is_tuple(Tuple) ->
    %% TODO: support break
    Recur =
        fun Recur (I, Acc) when I > tuple_size(Tuple) ->
                list_to_tuple(lists:reverse(Acc));
            Recur (I, Acc) when I > MaxWidth ->
                list_to_tuple(lists:reverse(['...' | Acc])); % TODO(?): omitted size
            Recur (I, Acc) ->
                Recur(I + 1, [adjust_data(element(I, Tuple), Depth + 1, MaxDepth, MaxWidth) | Acc])
        end,
    Recur(1, []);
adjust_data(Map, Depth, MaxDepth, MaxWidth) when is_map(Map) ->
    maps:fold(
      fun (Key0, Val0, Acc) ->
              case maps:size(Acc) of
                  Size when Size > MaxWidth   -> Acc;
                  Size when Size =:= MaxWidth -> maps:put('...', '...', Acc); % TODO: key conflict check
                  _                           ->
                      Key1 = adjust_data(Key0, Depth + 1, MaxDepth, MaxWidth),
                      Val1 = adjust_data(Val0, Depth + 1, MaxDepth, MaxWidth),
                      maps:put(Key1, Val1, Acc)
              end
      end,
      #{},
      Map);
adjust_data(Bin, _, _, MaxWidth) when is_binary(Bin) ->
    case Bin of
        <<Prefix:MaxWidth/binary, _/binary>> -> <<Prefix/binary, "...">>;
        _                                    -> Bin
    end;
adjust_data(X, _, _, _) ->
    %% non recursive type
    X.

%% TODO: optimize
abbreviate(IoData, Size) ->
    <<Text:Size/binary, _/binary>> = iolist_to_binary(IoData),
    [Text, <<"...">>].
