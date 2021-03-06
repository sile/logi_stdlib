%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Pairing Heaps
%% @private
%% @end
-module(logi_util_heap).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0, is_empty/1, in/2, out/1, peek/1, merge/2]).

-export_type([heap/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-opaque heap(Item) :: empty | {Item::tuple(), [heap(Item)]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc Returns an empty heap
-spec new() -> heap(_Item).
new() -> empty.

%% @doc Tests if `Heap' is empty and returns `true' if so and `false' otherwise
-spec is_empty(Heap :: heap(_Item)) -> boolean().
is_empty(empty) -> true;
is_empty(_)     -> false.

%% @doc Inserts `Item' into the heap `Heap'
%%
%% Returns the resulting heap
-spec in(Item, heap(Item)) -> heap(Item).
in(Item, Heap) -> merge({Item, []}, Heap).

%% @doc Removes the smallest item from the heap `Heap'
%%
%% Returns the `Heap2', where `Heap2' is the resulting heap.
%% If `Heap' is empty, the `empty' is returned.
-spec out(Heap :: heap(Item)) -> (Heap2 :: heap(Item)) | empty.
out(empty)     -> empty;
out({_, Heap}) -> merge_pairs(Heap).

%% @doc Returns the tuple `Item' where `Item' is the front item of `Heap', or `empty' if `Heap' is empty
-spec peek(Heap :: heap(Item)) -> Item | empty.
peek(empty)     -> empty;
peek({Item, _}) -> Item.

%% @doc Returns the merged heap of `Heap1' and `Heap2'
-spec merge(Heap1 :: heap(Item1), Heap2 :: heap(Item2)) -> heap(Item1|Item2).
merge(H, empty) -> H;
merge(empty, H) -> H;
merge(H1 = {X, Hs1}, H2 = {Y, Hs2}) ->
    case X < Y of
        true  -> {X, [H2 | Hs1]};
        false -> {Y, [H1 | Hs2]}
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec merge_pairs([heap(Item)]) -> heap(Item).
merge_pairs([])            -> empty;
merge_pairs([H])           -> H;
merge_pairs([H1, H2 | Hs]) -> merge(merge(H1, H2), merge_pairs(Hs)).
