-module(test_hash_map).
-author("vitaliy").

-include_lib("eunit/include/eunit.hrl").

remove_test() ->
  HashMap = hash_map:from_key_value_list([{kek, 1}, {lol, 2}, {arbidol, 3}]),
  RemovedHashMap = hash_map:remove(kek, HashMap),
  ?assert(hash_map:get(lol, RemovedHashMap) == 2),
  ?assert(hash_map:get(kek, RemovedHashMap) == false).

