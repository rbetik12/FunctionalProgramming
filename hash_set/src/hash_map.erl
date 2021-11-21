-module(hash_map).
-author("vitaliy").

%% API
-export([
  new/0,
  append/3,
  append_with_hash/4,
  get/2,
  get_with_hash/3,
  remove/2,
  from_key_value_list/1,
  from_key_value_list_with_hash/2,
  find/2,
  without/2
]).

-define(buckets_amount, 512).

-record(hash_map, {buckets}).
-record(hash_map_entry, {key, value}).

make_buckets(0, Buckets) -> Buckets;

make_buckets(Size, Buckets) ->
  make_buckets(Size - 1, lists:append(Buckets, [[]])).

new() -> #hash_map{buckets = make_buckets(?buckets_amount, [])}.

from_key_value_list(List) ->
  lists:foldl(
    fun(Item, HashMap) ->
      append(element(1, Item), element(2, Item), HashMap)
    end,
    new(),
    List
  ).

append_impl(Key, Value, HashFunction, #hash_map{buckets = Buckets}) ->
  Slot = HashFunction(Key, ?buckets_amount),
  #hash_map{buckets = lists:sublist(Buckets, Slot - 1) ++
    [bucket_modify(lists:nth(Slot, Buckets), Key, Value)] ++
    lists:sublist(Buckets, Slot + 1, ?buckets_amount - Slot + 1)}.

get_impl(Key, HashFunction, #hash_map{buckets = Buckets}) ->
  Slot = HashFunction(Key, ?buckets_amount),
  element(3, bucket_get(lists:nth(Slot, Buckets), Key)).

append(Key, Value, #hash_map{buckets = _} = HashMap) -> append_impl(Key, Value, fun erlang:phash2/2, HashMap).

get(Key, #hash_map{buckets = _} = HashMap) -> get_impl(Key, fun erlang:phash2/2, HashMap).

find(Key, #hash_map{buckets = Buckets}) ->
  Slot = erlang:phash2(Key, ?buckets_amount),
  bucket_find(lists:nth(Slot, Buckets), Key).

remove(Key, #hash_map{buckets = Buckets}) ->
  Slot = erlang:phash2(Key, ?buckets_amount),
  #hash_map{buckets = lists:sublist(Buckets, Slot - 1) ++
    [bucket_modify(lists:nth(Slot, Buckets), Key)] ++
    lists:sublist(Buckets, Slot + 1, ?buckets_amount - Slot + 1)}.

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
    false -> {false, false, false};
    Result -> Result
  end.

bucket_find(Bucket, Key) ->
  case lists:keyfind(Key, 2, Bucket) of
    false -> false;
    _ -> true
  end.

%% Functions for unit tests. Use with caution! %%

from_key_value_list_with_hash(List, HashFunction) ->
  lists:foldl(
    fun(Item, HashMap) ->
      append_with_hash(element(1, Item), element(2, Item), HashFunction, HashMap)
    end,
    new(),
    List
  ).

append_with_hash(Key, Value, HashFunction, #hash_map{buckets = _} = HashMap) ->
  append_impl(Key, Value, HashFunction, HashMap).

get_with_hash(Key, HashFunction, #hash_map{buckets = _} = HashMap) -> get_impl(Key, HashFunction, HashMap).

