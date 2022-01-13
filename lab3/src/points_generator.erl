-module(points_generator).
-behaviour(gen_server).
-author("vitaliy").

-export([start_link/2, send_new_function/4]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {output_gen_pid = 0, delta}).

start_link(Delta, no_message_passing) ->
  {_, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [Delta, no_message_passing], []),
  Pid;

start_link(Delta, OutputGeneratorPid) ->
  {_, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [Delta, OutputGeneratorPid], []),
  Pid.

send_new_function(Pid, X1, X2, Func) -> transport_message(Pid, {{X1, X2}, Func}).

init([Delta, no_message_passing]) -> {ok, #state{delta = Delta}};

init([Delta, OutputGeneratorPid]) -> {ok, #state{output_gen_pid = OutputGeneratorPid, delta = Delta}}.

handle_call({{X1, X2}, Func}, _, #state{output_gen_pid = Pid, delta = Delta}) ->
  List = lists:seq(1, abs(round((X2 - X1) / Delta))),
  {_, InterpolatedPoints} = lists:foldl(
    fun(_, {Acc, PointsList}) ->
      X = Acc,
      Y = Func(Acc),
      math_logger:log_point(Pid, X, Y),
      {Acc + Delta, PointsList ++ [{X, Y}]}
    end,
    {X1, []},
    List
  ),
  {reply, InterpolatedPoints, #state{output_gen_pid = Pid, delta = Delta}}.

handle_cast(_, _) -> throw("points_generator doesn't support gen_server casts").

transport_message(Pid, Message) -> gen_server:call(Pid, Message).


