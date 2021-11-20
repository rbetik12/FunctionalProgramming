-module(hash_map).
-author("vitaliy").

%% API
-export([new/0, append/3, get/2]).

-define(buckets_amount, 16).

-record(hash_map, {buckets}).
-record(hash_map_entry, {key, value}).

make_buckets(0, Buckets) -> Buckets;

make_buckets(Size, Buckets) ->
  make_buckets(Size - 1, lists:append(Buckets, [[]])).

new() -> #hash_map{buckets = make_buckets(?buckets_amount, [])}.

append(Key, Value, #hash_map{buckets = Buckets}) ->
  Slot = erlang:phash(Key, ?buckets_amount),
  #hash_map{buckets = lists:sublist(Buckets, Slot - 1) ++
    [bucket_modify(lists:nth(Slot, Buckets), Key, Value)] ++
    lists:sublist(Buckets, Slot + 1, ?buckets_amount - Slot + 1)}.

get(Key, #hash_map{buckets = Buckets}) ->
  Slot = erlang:phash(Key, ?buckets_amount),
  bucket_get(lists:nth(Slot, Buckets), Key).

bucket_modify(Bucket, Key, Value) ->
  case lists:keyfind(Key, 2, Bucket) of
    false -> lists:append(Bucket, [#hash_map_entry{key = Key, value = Value}]);
    _ -> lists:keyreplace(Key, 2, Bucket, #hash_map_entry{key = Key, value = Value})
  end.

bucket_get(Bucket, Key) ->
  case lists:keyfind(Key, 2, Bucket) of
    false -> false;
    Result -> Result
  end.

