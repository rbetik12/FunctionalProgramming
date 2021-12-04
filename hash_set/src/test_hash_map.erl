-module(test_hash_map).
-author("vitaliy").

-include_lib("eunit/include/eunit.hrl").

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
  ?assert(hash_map:get(kek, ModifiedHashMap) == {ok, 5}).

custom_hash_function(_, _) -> 1.

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

