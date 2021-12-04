-module(hash_set).
-author("vitaliy").

%% API
-export([
  new/0,
  put/2,
  print/1,
  remove/2,
  get_list/1,
  filter/2,
  map/2,
  foldr/3,
  foldl/3,
  from_list/1,
  compare/2,
  add/2,
  subtract/2
]).

-record(hash_set, {hash_map}).

new() -> #hash_set{hash_map = hash_map:new()}.

from_list(ListOfValues) ->
  lists:foldl(
    fun(Value, HashSet) ->
      hash_set:put(Value, HashSet)
    end,
    hash_set:new(),
    ListOfValues
  ).

compare(#hash_set{hash_map = _} = HashSet1, #hash_set{hash_map = _} = HashSet2) ->
  lists:sort(hash_set:get_list(HashSet1)) == lists:sort(hash_set:get_list(HashSet2)).

put(Element, #hash_set{hash_map = HashMap} = HashSet) ->
  case hash_map:find(Element, HashMap) of
    false -> #hash_set{hash_map = hash_map:append(Element, Element, HashMap)};
    _ -> HashSet
  end.

remove(Element, #hash_set{hash_map = HashMap} = HashSet) ->
  case hash_map:find(Element, HashMap) of
    none -> HashSet;
    _ -> #hash_set{hash_map = hash_map:remove(Element, HashMap)}
  end.

print(#hash_set{hash_map = HashMap}) -> io:format("~p~n", [hash_map:get_value_list(HashMap)]).

get_list(#hash_set{hash_map = HashMap}) -> hash_map:get_value_list(HashMap).

filter(Function, #hash_set{hash_map = HashMap}) ->
  ValueList = hash_map:get_value_list(HashMap),
  FilteredList = [X || X <- ValueList, Function(X)],
  DiffList = lists:subtract(ValueList, FilteredList),
  #hash_set{hash_map = hash_map:without(DiffList, HashMap)}.

fill_hash_map(ValuesList) ->
  lists:foldl(
    fun(Item, HashMap) ->
      hash_map:append(Item, Item, HashMap)
    end,
    hash_map:new(),
    ValuesList
  ).

map(Function, #hash_set{hash_map = HashMap}) ->
  MappedList = lists:map(Function, hash_map:get_value_list(HashMap)),
  #hash_set{hash_map = fill_hash_map(MappedList)}.

foldl(Function, Acc, #hash_set{hash_map = HashMap}) -> lists:foldl(Function, Acc, hash_map:get_value_list(HashMap)).

foldr(Function, Acc, #hash_set{hash_map = HashMap}) -> lists:foldr(Function, Acc, hash_map:get_value_list(HashMap)).

%% monoid functions %%

add(#hash_set{hash_map = HashMap1}, #hash_set{hash_map = HashMap2}) ->
  MergedList = hash_map:get_value_list(HashMap1) ++ hash_map:get_value_list(HashMap2),
  #hash_set{hash_map = fill_hash_map(MergedList)}.

subtract(#hash_set{hash_map = HashMap1}, #hash_set{hash_map = HashMap2}) ->
  SubtractedList = hash_map:get_value_list(HashMap1) -- hash_map:get_value_list(HashMap2),
  #hash_set{hash_map = fill_hash_map(SubtractedList)}.



