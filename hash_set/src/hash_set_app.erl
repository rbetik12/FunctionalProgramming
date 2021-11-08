-module(hash_set_app).

-behaviour(application).

-export([start/0, stop/1]).

start() ->
  HashSet = hash_set:new(),
  HashSet_ = hash_set:put(2, HashSet),
  HashSet__ = hash_set:put(3, hash_set:put(3, hash_set:put(1, HashSet_))),
%%  hash_set:print(hash_set:map(
%%    fun(X) ->
%%      case X > 0 of
%%        true -> -X;
%%        _ -> X
%%      end
%%    end,
%%    HashSet__)).
  hash_set:foldl(fun (X, Acc) -> X + Acc end, 0, HashSet__).

stop(_State) ->
  ok.
