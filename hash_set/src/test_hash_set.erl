-module(test_hash_set).
-author("vitaliy").

-include_lib("eunit/include/eunit.hrl").

filter_test() ->
  FilteredHashSet = hash_set:filter(fun(X) -> X rem 2 == 0 end, hash_set:from_list([4, -2, 1])),
  CorrectHashSet = hash_set:from_list([-2, 4]),
  ?assert(hash_set:compare(FilteredHashSet, CorrectHashSet) == true).

map_test() ->
  MappedHashSet = hash_set:map(
    fun(X) ->
      case X > 0 of
        true -> -X;
        _ -> X
      end
    end,
    hash_set:from_list([1, -3])),
  CorrectHashSet = hash_set:from_list([-1, -3]),
  ?assert(hash_set:compare(MappedHashSet, CorrectHashSet) == true).

fold_test() ->
  HashSet = hash_set:from_list([3, -2, 5, 1]),
  ?assert(hash_set:foldl(fun(X, Acc) -> X + Acc end, 0, HashSet) == 7),
  ?assert(hash_set:foldr(fun(X, Acc) -> X + Acc end, 0, HashSet) == 7).

filter_atomics_test() ->
  HashSet = hash_set:from_list([lol, kek, chebyrek]),
  FilteredHashSet = hash_set:filter(
    fun(X) ->
      case X of
        kek -> true;
        _ -> false
      end
    end, HashSet),
  ?assert(hash_set:compare(FilteredHashSet, hash_set:from_list([kek]))).

filter_then_add_element_test() ->
  HashSet = hash_set:from_list([lol, kek, chebyrek]),
  FilteredHashSet = hash_set:filter(
    fun(X) ->
      case X of
        kek -> true;
        _ -> false
      end
    end, HashSet),
  AddedHashSet = hash_set:put(lol, FilteredHashSet),
  ?assert(hash_set:compare(AddedHashSet, hash_set:from_list([kek, lol])) == true).

remove_different_types_test() ->
  HashSet = hash_set:from_list(["345", kek, lol]),
  NewHashSet = hash_set:remove("345", HashSet),
  ?assert(hash_set:compare(NewHashSet, hash_set:from_list([kek, lol]))).

%% Property-based testing %%

add_test() ->
  HashSet1 = hash_set:from_list([kek, lol]),
  HashSet2 = hash_set:from_list([1, 2, 3]),
  ResultHashSet = hash_set:add(HashSet1, HashSet2),
  InverseResultHashSet = hash_set:add(HashSet2, HashSet1),
  ?assert(hash_set:compare(ResultHashSet, InverseResultHashSet)).

subtract_test() ->
  HashSet1 = hash_set:from_list([kek, lol, 1]),
  HashSet2 = hash_set:from_list([1, 2, 3]),
  ResultHashSet = hash_set:subtract(HashSet1, HashSet2),
  InverseResultHashSet = hash_set:subtract(HashSet2, HashSet1),
  ?assert(hash_set:compare(ResultHashSet, InverseResultHashSet) == false).

zero_element_test() ->
  ZeroHashSet = hash_set:from_list([]),
  HashSet = hash_set:from_list([1, 2, 3]),
  ?assert(hash_set:compare(ZeroHashSet, ZeroHashSet)),
  ?assert(hash_set:compare(hash_set:add(ZeroHashSet, HashSet), hash_set:add(HashSet, ZeroHashSet)) == true),
  ?assert(hash_set:compare(hash_set:subtract(ZeroHashSet, HashSet), ZeroHashSet) == true),
  ?assert(hash_set:compare(hash_set:subtract(HashSet, ZeroHashSet), HashSet) == true).

