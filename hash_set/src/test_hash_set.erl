-module(test_hash_set).
-author("vitaliy").

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%put_test() ->
%%  HashSet = hash_set:new(),
%%  PutHashSet = hash_set:put(1, HashSet),
%%  ?assert(hash_set:get_list(PutHashSet) == [1]).
%%
%%remove_test() ->
%%  HashSet = hash_set:new(),
%%  PutHashSet = hash_set:put(1, HashSet),
%%  RemoveHashSet = hash_set:remove(1, PutHashSet),
%%  ?assert(hash_set:get_list(RemoveHashSet) == []).
%%
%%from_list_test() ->
%%  HashSetFromList = hash_set:from_list([1]),
%%  HashSet = hash_set:put(1, hash_set:new()),
%%  ?assert(hash_set:get_list(HashSetFromList) == hash_set:get_list(HashSet)).
%%
%%compare_test() ->
%%  HashSet1 = hash_set:from_list([2, 3, 4]),
%%  HashSet2 = hash_set:from_list([3, 4, 2]),
%%  ?assert(hash_set:compare(HashSet1, HashSet2) == true).
%%
%%filter_test() ->
%%  FilteredHashSet = hash_set:filter(fun(X) -> X rem 2 == 0 end, hash_set:from_list([4, -2, 1])),
%%  CorrectHashSet = hash_set:from_list([-2, 4]),
%%  ?assert(hash_set:compare(FilteredHashSet, CorrectHashSet) == true).
%%
%%map_test() ->
%%  MappedHashSet = hash_set:map(
%%    fun(X) ->
%%      case X > 0 of
%%        true -> -X;
%%        _ -> X
%%      end
%%    end,
%%    hash_set:from_list([1, -3])),
%%  CorrectHashSet = hash_set:from_list([-1, -3]),
%%  ?assert(hash_set:compare(MappedHashSet, CorrectHashSet) == true).
%%
%%fold_test() ->
%%  HashSet = hash_set:from_list([3, -2, 5, 1]),
%%  ?assert(hash_set:foldl(fun(X, Acc) -> X + Acc end, 0, HashSet) == 7),
%%  ?assert(hash_set:foldr(fun(X, Acc) -> X + Acc end, 0, HashSet) == 7).
%%
%%filter_atomics_test() ->
%%  HashSet = hash_set:from_list([lol, kek, chebyrek]),
%%  FilteredHashSet = hash_set:filter(
%%    fun(X) ->
%%      case X of
%%        kek -> true;
%%        _ -> false
%%      end
%%    end, HashSet),
%%  ?assert(hash_set:compare(FilteredHashSet, hash_set:from_list([kek]))).
%%
%%filter_then_add_element_test() ->
%%  HashSet = hash_set:from_list([lol, kek, chebyrek]),
%%  FilteredHashSet = hash_set:filter(
%%    fun(X) ->
%%      case X of
%%        kek -> true;
%%        _ -> false
%%      end
%%    end, HashSet),
%%  AddedHashSet = hash_set:put(lol, FilteredHashSet),
%%  ?assert(hash_set:compare(AddedHashSet, hash_set:from_list([kek, lol])) == true).
%%
%%remove_different_types_test() ->
%%  HashSet = hash_set:from_list(["345", kek, lol]),
%%  NewHashSet = hash_set:remove("345", HashSet),
%%  ?assert(hash_set:compare(NewHashSet, hash_set:from_list([kek, lol]))).
%%
%%%% Property-based testing %%
%%
%%prop_addition_commutativity() ->
%%  ?FORALL(
%%    {L1, L2},
%%    {list(integer()), list(integer())},
%%    begin
%%      HashSet1 = hash_set:from_list(L1),
%%      HashSet2 = hash_set:from_list(L2),
%%      Result = hash_set:add(HashSet1, HashSet2),
%%      InverseResult = hash_set:add(HashSet2, HashSet1),
%%      hash_set:compare(Result, InverseResult)
%%    end
%%  ).
%%
%%prop_subtraction_not_commutative() ->
%%  ?FORALL(
%%    {L1, L2},
%%    {list(integer()), list(integer())},
%%    begin
%%      HashSet1 = hash_set:from_list(L1),
%%      HashSet2 = hash_set:from_list(L2),
%%      Result = hash_set:subtract(HashSet1, HashSet2),
%%      InverseResult = hash_set:subtract(HashSet2, HashSet1),
%%      case hash_set:compare(HashSet1, HashSet2) == true of
%%        true -> true;
%%        _ -> hash_set:compare(Result, InverseResult) == false
%%      end
%%    end
%%  ).
%%
%%prop_addition_zero_element_commutativity() ->
%%  ?FORALL(
%%    {L1, L2},
%%    {list(), list(integer())},
%%    begin
%%      ZeroHashSet = hash_set:from_list(L1),
%%      HashSet = hash_set:from_list(L2),
%%      hash_set:compare(hash_set:add(ZeroHashSet, HashSet), hash_set:add(HashSet, ZeroHashSet))
%%    end
%%  ).
%%
%%prop_subtract_zero_element_commutativity() ->
%%  ?FORALL(
%%    {L1, L2},
%%    {list(), list(integer())},
%%    begin
%%      ZeroHashSet = hash_set:from_list(L1),
%%      HashSet = hash_set:from_list(L2),
%%      case L1 == L2 of
%%        true -> true;
%%        _ -> hash_set:compare(hash_set:subtract(ZeroHashSet, HashSet), hash_set:subtract(HashSet, ZeroHashSet)) == false
%%      end
%%    end
%%  ).
%%
%%add_commutative_test() ->
%%  ?assert(proper:quickcheck(prop_addition_commutativity(), [{to_file, user}, {numtests, 1000}]) == true).
%%
%%subtract_not_commutative_test() ->
%%  ?assert(proper:quickcheck(prop_subtraction_not_commutative(), [{to_file, user}, {numtests, 1000}]) == true).
%%
%%add_zero_element_commutativity_test() ->
%%  ?assert(proper:quickcheck(prop_addition_zero_element_commutativity(), [{to_file, user}, {numtests, 1000}]) == true).
%%
%%subtract_zero_element_commutativity_test() ->
%%  ?assert(proper:quickcheck(prop_subtract_zero_element_commutativity(), [{to_file, user}, {numtests, 1000}]) == true).

