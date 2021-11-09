-module(hash_set).
-author("vitaliy").

%% API
-export([new/0, put/2, print/1, remove/2, get_list/1, filter/2, map/2, foldr/3, foldl/3]).

-record(hash_set, {list, hash_map}).

new() -> #hash_set{list = [], hash_map = maps:new()}.

put(Element, #hash_set{list = List, hash_map = HashMap} = HashSet) ->
  case maps:get(Element, HashMap, none) of
    none -> #hash_set{list = List ++ [Element], hash_map = maps:put(Element, Element, HashMap)};
    _ -> HashSet
  end.

remove(Element, #hash_set{list = List, hash_map = HashMap} = HashSet) ->
  case maps:get(Element, HashMap, none) of
    none -> HashSet;
    _ -> #hash_set{list = lists:delete(Element, List), hash_map = maps:remove(Element, HashMap)}
  end.

print(#hash_set{list = List, hash_map = _}) -> io:format("~p~n", [List]).

get_list(#hash_set{list = List, hash_map = _}) -> List.

filter(Function, #hash_set{list = List, hash_map = HashMap}) ->
  FilteredList = [X || X <- List, Function(X)],
  DiffList = lists:subtract(List, FilteredList),
  #hash_set{list = FilteredList, hash_map = maps:without(DiffList, HashMap)}.

clear_dup(OldList, ClearedList, HashMap) ->
  case length(OldList) of
    0 -> #hash_set{list = ClearedList, hash_map = HashMap};
    _ ->
      Element = lists:nth(1, OldList),
      case maps:get(Element, HashMap, none) of
        none -> clear_dup(OldList -- [Element], ClearedList ++ [Element], maps:put(Element, Element, HashMap));
        _ -> clear_dup(OldList -- [Element], ClearedList, HashMap)
      end
  end.

map(Function, #hash_set{list = List, hash_map = _}) ->
  MappedList = lists:map(Function, List),
  clear_dup(MappedList, [], maps:new()).

foldl(Function, Acc, #hash_set{list = List, hash_map = _}) -> lists:foldl(Function, Acc, List).

foldr(Function, Acc, #hash_set{list = List, hash_map = _}) -> lists:foldr(Function, Acc, List).



