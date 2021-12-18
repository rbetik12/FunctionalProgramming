-module(points_generator).
-behaviour(gen_server).
-author("vitaliy").

-export([start_link/2, send_message/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {output_gen_pid, delta}).

start_link(Delta, OutputGeneratorPid) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Delta, OutputGeneratorPid], []).

init([Delta, OutputGeneratorPid]) -> {ok, #state{output_gen_pid = OutputGeneratorPid, delta = Delta}}.

handle_call({{X1, X2}, Func}, _, #state{output_gen_pid = Pid, delta = Delta}) ->
  List = lists:seq(1, abs(round((X2 - X1) / Delta))),
  {_, InterpolatedPoints} = lists:foldl(
    fun(_, {Acc, PointsList}) ->
      X = Acc,
      Y = Func(Acc),
      output_generator:send_message(Pid, {X, Y}),
      {Acc + Delta, PointsList ++ [{X, Y}]}
    end,
    {X1, []},
    List
  ),
  {reply, InterpolatedPoints, #state{output_gen_pid = Pid, delta = Delta}}.

handle_cast(_, _) -> throw("Cast in points_generator!").

send_message(Pid, Message) -> gen_server:call(Pid, Message).


