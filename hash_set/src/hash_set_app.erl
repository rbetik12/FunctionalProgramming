-module(hash_set_app).

-behaviour(application).

-export([start/0, stop/1]).

start() ->
    HashSet = hash_set:new(),
    HashSet_ = hash_set:put(2, HashSet),
    HashSet__ = hash_set:put(3, HashSet_),
    hash_set:print(hash_set:filter(fun (X) -> X rem 2 == 0 end, HashSet__)).

stop(_State) ->
    ok.
