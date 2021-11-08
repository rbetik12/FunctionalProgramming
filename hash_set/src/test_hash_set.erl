-module(test_hash_set).
-author("vitaliy").

-include_lib("eunit/include/eunit.hrl").

new_test() ->
  HashSet = hash_set:new(),
  ?assert(maps:size(element(3, HashSet)) == 0),
  ?assert(length(element(2, HashSet)) == 0).

dup_put_test() ->
  HashSet = hash_set:new(),
  HashSet_ = hash_set:put(2, HashSet),
  HashSet__ = hash_set:put(2, HashSet_),
  ?assert(hash_set:get_list(HashSet__) == [2]).

del_test() ->
  HashSet = hash_set:new(),
  HashSet_ = hash_set:put(2, HashSet),
  HashSet__ = hash_set:remove(2, HashSet_),
  ?assert(hash_set:get_list(HashSet__) == []).

filter_test() ->
  HashSet = hash_set:put(4, hash_set:put(-2, hash_set:put(1, hash_set:new()))),
  ?assert(hash_set:get_list(hash_set:filter(fun (X) -> X rem 2 == 0 end, HashSet)) == [-2, 4]).

map_test() ->
  HashSet = hash_set:put(-3, hash_set:put(3, hash_set:put(-1, hash_set:put(1, hash_set:new())))),
  ?assert(hash_set:get_list(hash_set:map(
    fun(X) ->
      case X > 0 of
        true -> -X;
        _ -> X
      end
    end,
    HashSet)) == [-1, -3]).
