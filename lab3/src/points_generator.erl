-module(points_generator).
-behaviour(gen_server).
-author("vitaliy").

-export([start_link/1]).
-export([init/1, handle_call/3]).

start_link(Delta) ->
  Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [Delta], [Delta]),
  Return.

init([Delta]) ->
  {ok, Delta}.

handle_call({{X1, X2}, Func}, _, Delta) ->
  List = lists:seq(1, round((X2 - X1) / Delta)),
  lists:foldl(
    fun(_, Acc) ->
      gen_server:call(output_generator, {Acc, Func(Acc)}),
      Acc + Delta
    end,
    X1,
    List
  ),
  {reply, ok, Delta};

handle_call(_, _, Delta) ->
  {reply, ok, Delta}.

