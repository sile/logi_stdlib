%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% NOTE: headers is not limited
%%
%% @doc TODO
-module(logi_layout_limit).

-behaviour(logi_layout).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1, new/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([format/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(LINE_LENGTH, "1000000000"). % 1GB
-define(IS_POS_INT(X), (is_integer(X) andalso X > 0)).

-record(extra, % TODO: ?STATE
        {
          base_layout       :: logi_layout:layout(),
          max_width = 512   :: pos_integer() | infinity,
          max_depth = 8     :: pos_integer() | infinity, % TODO: リストは対象から外しても良いかも？(ヒューリスティック)
          max_size  = 10240 :: pos_integer() | infinity
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(BaseLayout, [])
-spec new(logi_layout:layout()) -> logi_layout:layout().
new(BaseLayout) -> new(BaseLayout, []).

-spec new(logi_layout:layout(), Options) -> logi_layout:layout() when
      Options :: [Option],
      Option  :: {max_width, pos_integer() | infinity}
               | {max_depth, pos_integer() | infinity}
               | {max_size,  pos_integer() | infinity}.
new(BaseLayout, Options) ->
    Args = [BaseLayout, Options],
    _ = logi_layout:is_layout(BaseLayout) orelse error(badarg, Args),
    _ = is_list(Options) orelse error(badarg, Args),

    MaxWidth = proplists:get_value(max_width, Options, 512),
    MaxDepth = proplists:get_value(max_depth, Options, 8),
    MaxSize = proplists:get_value(max_size, Options, 10240),
    _ = ?IS_POS_INT(MaxWidth) orelse MaxWidth =:= infinity orelse error(badarg, Args),
    _ = ?IS_POS_INT(MaxDepth) orelse MaxDepth =:= infinity orelse error(badarg, Args),
    _ = ?IS_POS_INT(MaxSize) orelse MaxSize =:= infinity orelse error(badarg, Args),

    Extra =
        #extra{
           base_layout = BaseLayout,
           max_width   = MaxWidth,
           max_depth   = MaxDepth,
           max_size    = MaxSize
          },
    logi_layout:new(?MODULE, Extra).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
format(Context, Format0, Data0, Extra) ->
    Format1 = adjust_format(Format0),
    Data1 = [case is_truncation_needed(X, Extra#extra.max_depth, Extra#extra.max_width) of
                 false -> X;
                 true  -> adjust_data(X, 0, Extra#extra.max_depth, Extra#extra.max_width)
             end || X <- Data0],
    FormattedData = logi_layout:format(Context, Format1, Data1, Extra#extra.base_layout),
    case iolist_size(FormattedData) =< Extra#extra.max_size of
        true  -> FormattedData;
        false -> abbreviate(FormattedData, Extra#extra.max_size)
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec adjust_format(io:format()) -> io:format().
adjust_format(Format) when is_atom(Format)   -> adjust_format(atom_to_list(Format));
adjust_format(Format) when is_binary(Format) -> adjust_format(binary_to_list(Format));
adjust_format([])                            -> [];
adjust_format("~p" ++ Format)                -> "~" ?LINE_LENGTH "p" ++ adjust_format(Format);
adjust_format("~P" ++ Format)                -> "~" ?LINE_LENGTH "P" ++ adjust_format(Format);
adjust_format("~tp" ++ Format)               -> "~" ?LINE_LENGTH "tp" ++ adjust_format(Format);
adjust_format("~tP" ++ Format)               -> "~" ?LINE_LENGTH "tP" ++ adjust_format(Format);
adjust_format([$~, C | Format])              -> [$~, C | adjust_format(Format)];
adjust_format([C | Format])                  -> [C | adjust_format(Format)].

-spec is_truncation_needed(term(), pos_integer() | infinity, pos_integer() | infinity) -> boolean().
is_truncation_needed(X, MaxDepth, MaxWidth) ->
    try
        is_truncation_needed(X, 0, MaxDepth, MaxWidth)
    catch
        throw:break -> true
    end.

-spec is_truncation_needed(term(), non_neg_integer(), pos_integer() | infinity, pos_integer() | infinity) -> false.
is_truncation_needed(_, Depth, Depth, _) ->
    throw(break);
is_truncation_needed(List, Depth, MaxDepth, MaxWidth) when is_list(List) ->
    _ = lists:foldl(
          fun (_, I) when I =:= MaxWidth -> throw(break);
              (X, I)                     -> is_truncation_needed(X, Depth + 1, MaxDepth, MaxWidth), I + 1
          end,
          0,
          List),
    false;
is_truncation_needed(Tuple, Depth, MaxDepth, MaxWidth) when is_tuple(Tuple) ->
    Size = tuple_size(Tuple),
    case Size > MaxWidth of
        true  -> throw(break);
        false ->
            fun F (I) when I > Size -> false;
                F (I)               -> is_truncation_needed(element(I, Tuple), Depth + 1, MaxDepth, MaxWidth), F(I + 1)
            end(1)
    end;
is_truncation_needed(Map, Depth, MaxDepth, MaxWidth) when is_map(Map) ->
    case maps:size(Map) > MaxWidth of
        true  -> throw(break);
        false ->
            maps:fold(
              fun (Key, Val, _) ->
                      is_truncation_needed(Key, Depth + 1, MaxDepth, MaxWidth),
                      is_truncation_needed(Val, Depth + 1, MaxDepth, MaxWidth)
              end,
              false,
              Map)
    end;
is_truncation_needed(Bin, _, _, MaxWidth) when byte_size(Bin) > MaxWidth ->
    throw(break);
is_truncation_needed(_, _, _, _) ->
    false.

-spec adjust_data(term(), non_neg_integer(), pos_integer() | infinity, pos_integer() | infinity) -> term().
adjust_data(X, Depth, Depth, _) when is_list(X); is_tuple(X); is_map(X) ->
    '...';
adjust_data(List0, Depth, MaxDepth, MaxWidth) when is_list(List0) ->
    try
        {_, _, List1} =
            lists:foldl(
              fun (X, {I, NotString0, Acc}) ->
                      case I =:= MaxWidth of
                          true ->
                              case NotString0 of
                                  false -> throw({break, lists:reverse([<<"...">> | Acc])});
                                  true  -> throw({break, lists:reverse(['...' | Acc])})
                              end;
                          false ->
                              NotString1 = NotString0 orelse not (is_list(X) orelse is_binary(X) orelse is_integer(X)),
                              {I + 1, NotString1, [adjust_data(X, Depth + 1, MaxDepth, MaxWidth) | Acc]}
                      end
              end,
              {0, false, []},
              List0),
        lists:reverse(List1)
    catch
        throw:{break, List2} -> List2
    end;
adjust_data(Tuple0, Depth, MaxDepth, MaxWidth) when is_tuple(Tuple0) ->
    Size = tuple_size(Tuple0),
    try
        F =
            fun F (I, Acc) when I > Size     -> list_to_tuple(lists:reverse(Acc));
                F (I, Acc) when I > MaxWidth -> throw({break, list_to_tuple(lists:reverse(['...' | Acc]))});
                F (I, Acc)                   -> F(I + 1, [adjust_data(element(I, Tuple0), Depth + 1, MaxDepth, MaxWidth) | Acc])
            end,
        F(1, [])
    catch
        throw:{break, Tuple1} -> Tuple1
    end;
adjust_data(Map0, Depth, MaxDepth, MaxWidth) when is_map(Map0) ->
    try
        maps:fold(
          fun (Key0, Val0, Acc) ->
                  case maps:size(Acc) =:= MaxWidth of
                      true  -> throw({break, maps:put('...', '...', Acc)});
                      false ->
                          Key1 = adjust_data(Key0, Depth + 1, MaxDepth, MaxWidth),
                          Val1 = adjust_data(Val0, Depth + 1, MaxDepth, MaxWidth),
                          maps:put(Key1, Val1, Acc)
                  end
          end,
          #{},
          Map0)
    catch
        throw:{break, Map1} -> Map1
    end;
adjust_data(Bin, _, _, MaxWidth) when is_binary(Bin) ->
    case Bin of
        <<Prefix:MaxWidth/binary, _/binary>> -> [Prefix, <<"...">>];
        _                                    -> Bin
    end;
adjust_data(X, _, _, _) ->
    X. % A fixed length data

-spec abbreviate(iodata(), pos_integer()) -> iodata().
abbreviate(IoData, Size) ->
    <<Text:Size/binary, Rest/binary>> = iolist_to_binary(IoData),
    [Text, <<"...", (integer_to_binary(byte_size(Rest)))/binary, "...">>].
