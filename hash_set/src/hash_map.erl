-module(hash_map).
-author("vitaliy").

%% API
-export([
  new/0,
  new/1,
  append/3,
  get/2,
  remove/2,
  from_key_value_list/1,
  from_key_value_list_with_hash/2,
  find/2,
  without/2,
  get_key_value_list/1,
  get_value_list/1
]).

-record(hash_map, {buckets, buckets_amount, hash_function}).
-record(hash_map_entry, {key, value}).

new() -> #hash_map{buckets = [[], [], [], []], buckets_amount = 4, hash_function = fun erlang:phash2/2}.

new(HashFunction) -> #hash_map{buckets = [[], [], [], []], buckets_amount = 4, hash_function = HashFunction}.

from_key_value_list(List) ->
  lists:foldl(
    fun(Item, HashMap) ->
      append(element(1, Item), element(2, Item), HashMap)
    end,
    new(),
    List
  ).

get(Key, #hash_map{buckets = Buckets, buckets_amount = BucketsAmount, hash_function = HashFunction}) ->
%%  io:format("~p~n", [[Buckets, BucketsAmount, HashFunction]]),
  Slot = HashFunction(Key, BucketsAmount) + 1,
  bucket_get(lists:nth(Slot, Buckets), Key).

append(Key, Value, #hash_map{buckets = Buckets, buckets_amount = BucketsAmount, hash_function = HashFunction}) ->
%%  io:format("~p~n", [[Buckets, BucketsAmount, HashFunction]]),
  Slot = HashFunction(Key, BucketsAmount) + 1,
  #hash_map{
    buckets = lists:sublist(Buckets, Slot - 1) ++
    [bucket_modify(lists:nth(Slot, Buckets), Key, Value)] ++
    lists:sublist(Buckets, Slot + 1, BucketsAmount - Slot + 1),
    buckets_amount = BucketsAmount,
    hash_function = HashFunction}.

find(Key, #hash_map{buckets = Buckets, buckets_amount = BucketsAmount, hash_function = HashFunction}) ->
  Slot = HashFunction(Key, BucketsAmount) + 1,
  bucket_find(lists:nth(Slot, Buckets), Key).

remove(Key, #hash_map{buckets = Buckets, buckets_amount = BucketsAmount, hash_function = HashFunction}) ->
  Slot = HashFunction(Key, BucketsAmount) + 1,
  #hash_map{
    buckets = lists:sublist(Buckets, Slot - 1) ++
    [bucket_modify(lists:nth(Slot, Buckets), Key)] ++
    lists:sublist(Buckets, Slot + 1, BucketsAmount - Slot + 1),
    buckets_amount = BucketsAmount, hash_function = HashFunction}.

without(ListOfKeys, #hash_map{buckets = _} = HashMap) ->
  lists:foldl(
    fun (Key, HashMap) ->
      remove(Key, HashMap)
    end,
    HashMap,
    ListOfKeys
  ).

bucket_modify(Bucket, Key, Value) ->
  case lists:keyfind(Key, 2, Bucket) of
    false -> lists:append(Bucket, [#hash_map_entry{key = Key, value = Value}]);
    _ -> lists:keyreplace(Key, 2, Bucket, #hash_map_entry{key = Key, value = Value})
  end.

bucket_modify(Bucket, Key) ->
  lists:keydelete(Key, 2, Bucket).

bucket_get(Bucket, Key) ->
  case lists:keyfind(Key, 2, Bucket) of
    false -> {notfound, false};
    Result -> {ok, element(3, Result)}
  end.

bucket_find(Bucket, Key) ->
  case lists:keyfind(Key, 2, Bucket) of
    false -> false;
    _ -> true
  end.

from_key_value_list_with_hash(List, HashFunction) ->
  lists:foldl(
    fun(Item, HashMap) ->
      append(element(1, Item), element(2, Item), HashMap)
    end,
    new(HashFunction),
    List
  ).

get_key_value_list(#hash_map{buckets = Buckets, buckets_amount = _, hash_function = _}) ->
  lists:foldl(
    fun(Bucket, List) ->
      KeyValueList = lists:map(fun(Item) -> {element(2, Item), element(3, Item)} end, Bucket),
      [X || X <- List ++ KeyValueList]
    end,
    [],
    Buckets
  ).

get_value_list(#hash_map{buckets = _, buckets_amount = _, hash_function = _} = HashMap) ->
  KeyValueList = get_key_value_list(HashMap),
  lists:map(fun (KeyValueTuple) -> element(2, KeyValueTuple) end, KeyValueList).
