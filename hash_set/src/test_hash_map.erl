-module(test_hash_map).
-author("vitaliy").

-include_lib("eunit/include/eunit.hrl").

remove_test() ->
  HashMap = hash_map:from_key_value_list([{kek, 1}, {lol, 2}, {arbidol, 3}]),
  RemovedHashMap = hash_map:remove(kek, HashMap),
  ?assert(hash_map:get(lol, RemovedHashMap) == 2),
  ?assert(hash_map:get(kek, RemovedHashMap) == false).

change_element_test() ->
  HashMap = hash_map:from_key_value_list([{kek, 2}]),
  ModifiedHashMap = hash_map:append(kek, 5, HashMap),
  ?assert(hash_map:get(kek, ModifiedHashMap) == 5).

custom_hash_function(_, _) -> 1.

append_with_collision_test() ->
  HashMap = hash_map:from_key_value_list_with_hash([{kek, 2}, {lol, 5}], fun custom_hash_function/2),
  ?assert(hash_map:get_with_hash(kek, fun custom_hash_function/2, HashMap) == 2),
  ?assert(hash_map:get_with_hash(lol, fun custom_hash_function/2, HashMap) == 5).

