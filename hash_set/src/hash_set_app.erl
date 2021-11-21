-module(hash_set_app).

-behaviour(application).

-export([start/0, stop/1]).

start() ->
  HashSet = hash_set:from_list([1, -1]),
  HashSet2 = hash_set:from_list([1, -1, -1]),
%%  io:format("~p~n", [hash_set:get_list(hash_set:subtract(HashSet, HashSet2))]),
  hash_set:get_list(hash_set:subtract(HashSet2, HashSet)).
%%  io:format("~p~n", []).

stop(_State) ->
  ok.
