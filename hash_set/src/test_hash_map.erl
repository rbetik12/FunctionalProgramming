-module(test_hash_map).
-author("vitaliy").

-include_lib("eunit/include/eunit.hrl").

custom_hash_function(_, _) -> 1.

append_test() ->
  HashMap = hash_map:new(),
  AppendedHashMap = hash_map:append(1, 2, HashMap),
  ?assert(hash_map:get(1, AppendedHashMap) == {ok, 2}),
  ?assert(hash_map:get(2, AppendedHashMap) == {notfound, false}).

from_key_value_list_test() ->
  HashMap = hash_map:append(1, 2, hash_map:new()),
  HashMapFromList = hash_map:from_key_value_list([{1, 2}]),
  ?assert(hash_map:get(1, HashMap) == hash_map:get(1, HashMapFromList)),
  ?assert(hash_map:get(2, HashMap) == hash_map:get(2, HashMapFromList)),
  ?assert(hash_map:get(2, HashMap) == {notfound, false}).

remove_test() ->
  HashMap = hash_map:from_key_value_list([{kek, 1}, {lol, 2}, {arbidol, 3}]),
  RemovedHashMap = hash_map:remove(kek, HashMap),
  ?assert(hash_map:get(lol, RemovedHashMap) == {ok, 2}),
  ?assert(hash_map:get(kek, RemovedHashMap) == {notfound, false}).

change_element_test() ->
  HashMap = hash_map:from_key_value_list([{kek, 2}]),
  ModifiedHashMap = hash_map:append(kek, 5, HashMap),
  ?assert(hash_map:get(kek, ModifiedHashMap) == {ok, 2}).

append_with_collision_test() ->
  HashMap = hash_map:from_key_value_list_with_hash([{kek, 2}, {lol, 5}], fun custom_hash_function/2),
  ?assert(hash_map:get(kek, HashMap) == {ok, 2}),
  ?assert(hash_map:get(lol, HashMap) == {ok, 5}).

find_test() ->
  HashMap = hash_map:from_key_value_list([{kek, 1}, {lol, 2}, {arbidol, 3}]),
  ?assert(hash_map:find(kek, HashMap) == true),
  ?assert(hash_map:find(keke, HashMap) == false).

without_test() ->
  HashMap = hash_map:from_key_value_list([{kek, 2}, {lol, 4}]),
  WithoutHashMap = hash_map:without([kek, keke], HashMap),
  ?assert(hash_map:get(kek, WithoutHashMap) == {notfound, false}),
  ?assert(hash_map:get(keke, WithoutHashMap) == {notfound, false}),
  ?assert(hash_map:get(lol, WithoutHashMap) == {ok, 4}).

get_key_value_list_test() ->
  ListForHashMap = [{kek, 2}, {lol, 4}, {abc, 5}, {false, 57}],
  HashMap = hash_map:from_key_value_list(ListForHashMap),
  SortedActualList = lists:keysort(1, hash_map:get_key_value_list(HashMap)),
  SortedExpectedList = lists:keysort(1, ListForHashMap),
  ?assert(SortedActualList == SortedExpectedList).

get_value_list_test() ->
  HashMap = hash_map:from_key_value_list([{kek, 2}, {lol, 4}, {abc, 5}, {false, 57}]),
  SortedActualList = lists:sort(hash_map:get_value_list(HashMap)),
  SortedExpectedList = [2, 4, 5, 57],
  ?assert(SortedActualList == SortedExpectedList).

expand_test() ->
  HashMap = hash_map:from_key_value_list([{2, 3}, {false, 4}, {true, 5}]),
  HashMap1 = hash_map:append_list([{key1, 3}, {key2, 3}, {key3, 10}, {key4, 10}, {key5, 10}], HashMap),
  ?assert(hash_map:get(2, HashMap1) == {ok, 3}),
  ?assert(hash_map:get(false, HashMap1) == {ok, 4}),
  ?assert(hash_map:get(key3, HashMap1) == {ok, 10}).

empty_hash_map_test() ->
  HashMap = hash_map:append_list([], hash_map:new()),
  HashMap1 = hash_map:new(),
  ?assert(hash_map:get_value_list(HashMap) == []),
  ?assert(hash_map:get_value_list(HashMap1) == []).

get_key_list_test() ->
  HashMap = hash_map:from_key_value_list([{kek, 2}, {lol, 4}, {abc, 5}, {false, 57}]),
  SortedActualList = lists:sort(hash_map:get_key_list(HashMap)),
  SortedExpectedList = lists:sort([kek, lol, abc, false]),
  ?assert(SortedActualList == SortedExpectedList).

size_simple_test() ->
  HashMap = hash_map:from_key_value_list([{kek, 2}, {lol, 4}, {abc, 5}, {false, 57}]),
  ?assert(hash_map:size(HashMap) == 4).

size_append_remove_test() ->
  HashMap = hash_map:from_key_value_list([{kek, 2}, {lol, 4}, {abc, 5}, {false, 57}]),
  HashMapRemoved = hash_map:remove(kek, HashMap),
  ?assert(hash_map:size(HashMap) == 4),
  ?assert(hash_map:size(HashMapRemoved) == 3).

size_expanded_hash_map_test() ->
  HashMap = hash_map:from_key_value_list([{2, 3}, {false, 4}, {true, 5}]),
  HashMap1 = hash_map:append_list([{key1, 3}, {key2, 3}, {key3, 10}, {key4, 10}, {key5, 10}], HashMap),
  ?assert(hash_map:size(HashMap) == 3),
  ?assert(hash_map:size(HashMap1) == 8).

compare_with_same_keys_test() ->
  HashMap = hash_map:from_key_value_list([{key1, 3}, {key2, 4}, {key3, 5}]),
  HashMapNotEqual = hash_map:from_key_value_list([{key1, 4}, {key2, 5}, {key3, 6}]),
  ?assert(hash_map:compare(HashMap, HashMapNotEqual) == false),
  ?assert(hash_map:compare(HashMap, HashMap) == true).

compare_with_different_keys_test() ->
  HashMap = hash_map:from_key_value_list([{key1, 3}, {key2, 4}, {key3, 5}]),
  HashMapNotEqual = hash_map:from_key_value_list([{key4, 4}, {key5, 5}, {key6, 6}]),
  ?assert(hash_map:compare(HashMap, HashMapNotEqual) == false).

compare_with_same_keys_but_different_size_test() ->
  HashMap = hash_map:from_key_value_list([{key1, 3}, {key2, 4}]),
  HashMapNotEqual = hash_map:from_key_value_list([{key1, 4}, {key2, 5}, {key3, 6}]),
  ?assert(hash_map:compare(HashMap, HashMapNotEqual) == false),
  ?assert(hash_map:compare(HashMapNotEqual, HashMap) == false).

filter_test() ->
  HashMap = hash_map:from_key_value_list([{key1, 2}, {key2, 4}, {key3, 5}]),
  FilteredHashMap = hash_map:filter(fun (Item) -> Item rem 2 =/= 0 end, HashMap),
  FilteredHashMap1 = hash_map:filter(fun (Item) -> Item rem 2 == 0 end, HashMap),
  ?assert(hash_map:size(FilteredHashMap) == 1),
  ?assert(hash_map:get_value_list(FilteredHashMap) == [5]),
  ?assert(hash_map:size(FilteredHashMap1) == 2),
  ?assert(hash_map:get_value_list(FilteredHashMap1) == [2, 4]).

foldl_test() ->
  HashMap = hash_map:from_key_value_list([{key1, 2}, {key2, 4}, {key3, 5}]),
  Sum = hash_map:foldl(fun (Item, Sum) -> Sum + Item end, 0, HashMap),
  Prod = hash_map:foldl(fun (Item, Sum) -> Sum * Item end, 1, HashMap),
  ?assert(Sum == 11),
  ?assert(Prod == 40).

foldr_test() ->
  HashMap = hash_map:from_key_value_list([{key1, 2}, {key2, 4}, {key3, 5}]),
  Sum = hash_map:foldr(fun (Item, Sum) -> Sum + Item end, 0, HashMap),
  Prod = hash_map:foldr(fun (Item, Sum) -> Sum * Item end, 1, HashMap),
  ?assert(Sum == 11),
  ?assert(Prod == 40).

concat_test() ->
  KeyValueList1 = [{key1, 2}, {key2, 4}, {key3, 5}],
  KeyValueList2 = [{key4, 2}, {key5, 4}, {key6, 5}],
  HashMap1 = hash_map:from_key_value_list(KeyValueList1),
  HashMap2 = hash_map:from_key_value_list(KeyValueList2),
  ExpectedHashMap = hash_map:from_key_value_list(KeyValueList1 ++ KeyValueList2),
  ActualHashMap = hash_map:concat(HashMap1, HashMap2),
  ?assert(hash_map:compare(ExpectedHashMap, ActualHashMap) == true),
  ?assert(hash_map:size(ActualHashMap) == 6).

concat_same_test() ->
  KeyValueList1 = [{key1, 2}, {key2, 4}, {key3, 5}],
  HashMap1 = hash_map:from_key_value_list(KeyValueList1),
  ExpectedHashMap = hash_map:from_key_value_list(KeyValueList1),
  ActualHashMap = hash_map:concat(HashMap1, HashMap1),
  ?assert(hash_map:compare(ExpectedHashMap, ActualHashMap) == true),
  ?assert(hash_map:size(ActualHashMap) == 3).