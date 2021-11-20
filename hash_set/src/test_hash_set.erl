-module(test_hash_set).
-author("vitaliy").
%%
%%-include_lib("eunit/include/eunit.hrl").
%%
%%%% property-based testing %%
%%
%%new_test() ->
%%  HashSet = hash_set:new(),
%%  ?assert(maps:size(element(3, HashSet)) == 0),
%%  ?assert(length(element(2, HashSet)) == 0).
%%
%%dup_put_test() ->
%%  HashSet = hash_set:put(2, hash_set:put(2, hash_set:put(2, hash_set:put(2, hash_set:new())))),
%%  ?assert(hash_set:get_list(HashSet) == [2]).
%%
%%del_test() ->
%%  HashSet = hash_set:put(2, hash_set:remove(2, hash_set:put(2, hash_set:new()))),
%%  ?assert(hash_set:get_list(HashSet) == [2]).
%%
%%%% unit-testing %%
%%
%%filter_test() ->
%%  HashSet = hash_set:put(4, hash_set:put(-2, hash_set:put(1, hash_set:new()))),
%%  ?assert(hash_set:get_list(hash_set:filter(fun(X) -> X rem 2 == 0 end, HashSet)) == [-2, 4]).
%%
%%map_test() ->
%%  HashSet = hash_set:put(-3, hash_set:put(3, hash_set:put(-1, hash_set:put(1, hash_set:new())))),
%%  ?assert(hash_set:get_list(hash_set:map(
%%    fun(X) ->
%%      case X > 0 of
%%        true -> -X;
%%        _ -> X
%%      end
%%    end,
%%    HashSet)) == [-1, -3]).
%%
%%fold_test() ->
%%  HashSet = hash_set:put(3, hash_set:put(-2, hash_set:put(5, hash_set:put(1, hash_set:new())))),
%%  ?assert(hash_set:foldl(fun(X, Acc) -> X + Acc end, 0, HashSet) == 7),
%%  ?assert(hash_set:foldr(fun(X, Acc) -> X + Acc end, 0, HashSet) == 7).
%%
%%%% Let's break some stuff %%
%%
%%map_then_add_element_test() ->
%%  HashSet = hash_set:put(4, hash_set:put(3, hash_set:put(5, hash_set:put(2, hash_set:new())))),
%%  MappedHashSet = hash_set:map(
%%    fun(X) ->
%%      X + 2
%%    end,
%%    HashSet),
%%  ?assert(hash_set:get_list(hash_set:put(6, hash_set:remove(6, MappedHashSet))) == [4, 7, 5, 6]).
%%
%%filter_atomics_test() ->
%%  HashSet = hash_set:put(chebyrek, hash_set:put(lol, hash_set:put(kek, hash_set:new()))),
%%  FilteredHashSet = hash_set:filter(
%%    fun(X) ->
%%      case X of
%%        kek -> true;
%%        _ -> false
%%      end
%%    end, HashSet),
%%  ?assert(hash_set:get_list(FilteredHashSet) == [kek]).
%%
%%filter_then_add_element_test() ->
%%  HashSet = hash_set:put(chebyrek, hash_set:put(lol, hash_set:put(kek, hash_set:new()))),
%%  FilteredHashSet = hash_set:filter(
%%    fun(X) ->
%%      case X of
%%        kek -> true;
%%        _ -> false
%%      end
%%    end, HashSet),
%%  ?assert(hash_set:get_list(hash_set:put(lol, FilteredHashSet)) == [kek, lol]).
%%
%%remove_different_types_test() ->
%%  HashSet = hash_set:put("345", hash_set:put(2, hash_set:put(kek, hash_set:new()))),
%%  NewHashSet = hash_set:remove("345", HashSet),
%%  ?assert(hash_set:get_list(NewHashSet) == [kek, 2]).
%%
