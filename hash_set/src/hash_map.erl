-module(hash_map).
-author("vitaliy").

%% API
-export([new/0, append/3, get/2, remove/2, from_key_value_list/1]).

-define(buckets_amount, 16).

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

append(Key, Value, #hash_map{buckets = Buckets}) ->
  Slot = erlang:phash(Key, ?buckets_amount),
  #hash_map{buckets = lists:sublist(Buckets, Slot - 1) ++
    [bucket_modify(lists:nth(Slot, Buckets), Key, Value)] ++
    lists:sublist(Buckets, Slot + 1, ?buckets_amount - Slot + 1)}.

get(Key, #hash_map{buckets = Buckets}) ->
  Slot = erlang:phash(Key, ?buckets_amount),
  element(3, bucket_get(lists:nth(Slot, Buckets), Key)).

remove(Key, #hash_map{buckets = Buckets}) ->
  Slot = erlang:phash(Key, ?buckets_amount),
  #hash_map{buckets = lists:sublist(Buckets, Slot - 1) ++
    [bucket_modify(lists:nth(Slot, Buckets), Key)] ++
    lists:sublist(Buckets, Slot + 1, ?buckets_amount - Slot + 1)}.


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

