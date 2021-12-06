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
  List = lists:seq(1, abs(round((X2 - X1) / Delta))),
  {_, InterpolatedPoints} = lists:foldl(
    fun(_, {Acc, PointsList}) ->
      X = Acc,
      Y = Func(Acc),
      gen_server:call(output_generator, {X, Y}),
      {Acc + Delta, PointsList ++ [{X, Y}]}
    end,
    {X1, []},
    List
  ),
  {reply, InterpolatedPoints, Delta};

handle_call(_, _, Delta) ->
  {reply, ok, Delta}.

