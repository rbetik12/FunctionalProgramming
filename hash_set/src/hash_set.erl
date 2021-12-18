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

compare(#hash_set{hash_map = HashMap1}, #hash_set{hash_map = HashMap2}) -> hash_map:compare(HashMap1, HashMap2).

put(Element, #hash_set{hash_map = HashMap}) -> #hash_set{hash_map = hash_map:append(Element, Element, HashMap)}.

remove(Element, #hash_set{hash_map = HashMap}) -> #hash_set{hash_map = hash_map:remove(Element, HashMap)}.

print(#hash_set{hash_map = HashMap}) -> io:format("~p~n", [hash_map:get_value_list(HashMap)]).

get_list(#hash_set{hash_map = HashMap}) -> hash_map:get_value_list(HashMap).

filter(Function, #hash_set{hash_map = HashMap}) -> #hash_set{hash_map = hash_map:filter(Function, HashMap)}.

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

foldl(Function, Acc, #hash_set{hash_map = HashMap}) -> hash_map:foldl(Function, Acc, HashMap).

foldr(Function, Acc, #hash_set{hash_map = HashMap}) -> hash_map:foldr(Function, Acc, HashMap).

%% monoid functions %%

add(#hash_set{hash_map = HashMap1}, #hash_set{hash_map = HashMap2}) ->
  #hash_set{hash_map = hash_map:concat(HashMap1, HashMap2)}.

subtract(#hash_set{hash_map = HashMap1}, #hash_set{hash_map = HashMap2}) ->
  SubtractedList = hash_map:get_value_list(HashMap1) -- hash_map:get_value_list(HashMap2),
  #hash_set{hash_map = fill_hash_map(SubtractedList)}.



