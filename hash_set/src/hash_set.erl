-module(hash_set).
-author("vitaliy").

%% API
-export([new/0, put/2, print/1, remove/2, get_list/1, filter/2]).

-record(hash_set, {list, hash_map}).

new() -> #hash_set{list=[], hash_map = maps:new()}.

put(Element, #hash_set{list=List, hash_map = HashMap}=HashSet) ->
  case maps:get(Element, HashMap, none) of
    none -> #hash_set{list=List ++ [Element], hash_map = maps:put(Element, Element, HashMap)};
    _ -> HashSet
  end.

remove(Element, #hash_set{list=List, hash_map = HashMap}=HashSet) ->
  case maps:get(Element, HashMap, none) of
    none -> HashSet;
    _ -> #hash_set{list=lists:delete(Element, List), hash_map = maps:remove(Element, HashMap)}
  end.

print(#hash_set{list=List, hash_map = HashMap}=HashSet) -> io:format("~p~n", [List]).

get_list(#hash_set{list=List, hash_map = HashMap}=HashSet) -> List.

filter(Function, #hash_set{list=List, hash_map = HashMap}=HashSet) ->
  FilteredList = [X || X <- List, Function(X)],
  DiffList = lists:subtract(List, FilteredList),
  #hash_set{list = FilteredList, hash_map = maps:without(DiffList, HashMap)}.



