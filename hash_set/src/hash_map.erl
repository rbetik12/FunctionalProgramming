-module(hash_map).
-author("vitaliy").

%% API
-export([
  new/0,
  new/1,
  append/3,
  append_list/2,
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

new(Buckets, BucketsAmount, HashFunction) -> #hash_map{
  buckets = Buckets,
  buckets_amount = BucketsAmount,
  hash_function = HashFunction
}.

from_key_value_list(List) ->
  lists:foldl(
    fun(Item, HashMap) ->
      append(element(1, Item), element(2, Item), HashMap)
    end,
    new(),
    List
  ).

append_list(List, #hash_map{} = HashMap) ->
  lists:foldl(
    fun(Item, HashMap) ->
      append(element(1, Item), element(2, Item), HashMap)
    end,
    HashMap,
    List
  ).

get(Key, #hash_map{buckets = Buckets, buckets_amount = BucketsAmount, hash_function = HashFunction}) ->
  Slot = HashFunction(Key, BucketsAmount) + 1,
  bucket_get(lists:nth(Slot, Buckets), Key).

append(Key, Value, #hash_map{buckets = Buckets, buckets_amount = BucketsAmount, hash_function = HashFunction}) ->
  Slot = HashFunction(Key, BucketsAmount) + 1,

  Buckets1 = lists:sublist(Buckets, Slot - 1) ++
    [bucket_modify(lists:nth(Slot, Buckets), Key, Value)] ++
    lists:sublist(Buckets, Slot + 1, BucketsAmount - Slot + 1),

  HashMap1 = #hash_map{
    buckets = Buckets1,
    buckets_amount = BucketsAmount,
    hash_function = HashFunction},

  ShouldExpand = should_expand(lists:nth(Slot, Buckets), BucketsAmount),
  case ShouldExpand of
    true -> expand_hash_map(HashMap1);
    false -> HashMap1
  end.


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

without(ListOfKeys, #hash_map{} = HashMap) ->
  lists:foldl(
    fun(Key, HashMap) ->
      remove(Key, HashMap)
    end,
    HashMap,
    ListOfKeys
  ).

expand_hash_map(#hash_map{buckets = Buckets, buckets_amount = BucketsAmount, hash_function = HashFunction} = HashMap) ->
  ExpandedBuckets = lists:foldl(fun(_, Acc) -> Acc ++ [Acc1 || Acc1 <- [[], []]] end, [], Buckets),
  lists:foldl(
    fun(Item, HashMap1) ->
      append(element(1, Item), element(2, Item), HashMap1)
    end,
    new(ExpandedBuckets, BucketsAmount * 2, HashFunction),
    get_key_value_list(HashMap)
  ).

should_expand(Bucket, BucketsAmount) ->
  BucketPairsAmount = length(Bucket),
  case BucketPairsAmount of Amount
  when Amount / BucketsAmount >= 0.75 -> true;
    _ -> false
  end.

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

get_key_value_list(#hash_map{buckets = Buckets}) ->
  lists:foldl(
    fun(Bucket, List) ->
      KeyValueList = lists:map(fun(Item) -> {element(2, Item), element(3, Item)} end, Bucket),
      [X || X <- List ++ KeyValueList]
    end,
    [],
    Buckets
  ).

get_value_list(#hash_map{} = HashMap) ->
  KeyValueList = get_key_value_list(HashMap),
  lists:map(fun(KeyValueTuple) -> element(2, KeyValueTuple) end, KeyValueList).
