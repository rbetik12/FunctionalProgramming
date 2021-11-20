-module(hash_set_app).

-behaviour(application).

-export([start/0, stop/1]).

start() ->
  HashMap = hash_map:new(),
  NewHashMap = hash_map:append(key1, 5, HashMap),
  io:format("~p~n", [NewHashMap]),
  io:format("~p~n", [hash_map:get(key1, NewHashMap)]),
  RemovedHashMap = hash_map:remove(key1, NewHashMap),
  io:format("~p~n", [RemovedHashMap]).

stop(_State) ->
  ok.
