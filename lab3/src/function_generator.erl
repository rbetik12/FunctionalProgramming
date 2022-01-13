-module(function_generator).
-behaviour(gen_server).
-export([start_link/2, add_point/3]).
-export([init/1, handle_cast/2, handle_call/3]).

-record(state, {generator_type, points_gen_pid = 0, points_list = [], func_map = maps:new()}).

start_link(GeneratorType, no_message_passing) ->
  {_, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [GeneratorType, no_message_passing], []),
  Pid;

start_link(GeneratorType, PointsGenPid) ->
  {_, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [GeneratorType, PointsGenPid], []),
  Pid.

add_point(Pid, X, Y) -> transport_message(Pid, {add_point, X, Y}, cast).

init([GeneratorType, no_message_passing]) -> {ok, #state{generator_type = GeneratorType}};

init([GeneratorType, PointsGenPid]) -> {ok, #state{generator_type = GeneratorType, points_gen_pid = PointsGenPid}}.

handle_cast({add_point, X, Y}, #state{generator_type = linear} = State) -> handle_point(X, Y, 2, State);

handle_cast({add_point, X, Y}, #state{generator_type = quadratic} = State) -> handle_point(X, Y, 3, State).

handle_call(_, _, _) -> throw("This module doesn't support gen_server calls").

handle_point(X, Y, MaxPoints, #state{
  generator_type = Type,
  points_gen_pid = Pid,
  points_list = PointList,
  func_map = FuncMap}) ->
  PointList1 = lists:append(PointList, [{X, Y}]),
  case length(PointList1) of
    MaxPoints -> {
      noreply,
      #state{
        generator_type = Type,
        points_list = [lists:nth(MaxPoints, PointList1)],
        func_map = generate_function(Type, PointList1, FuncMap, Pid),
        points_gen_pid = Pid}
    };
    _ -> {
      noreply,
      #state{
        generator_type = Type,
        points_list = PointList1,
        func_map = FuncMap,
        points_gen_pid = Pid}
    }
  end.

generate_function(linear, PointsList, FuncMap, Pid) ->
  {X1, Y1} = lists:nth(1, PointsList),
  {X2, Y2} = lists:nth(2, PointsList),
  A1 = (Y2 - Y1) / (X2 - X1),
  A0 = Y1 - A1 * X1,
  Func = fun(X) -> A0 + A1 * X end,
  pass_message(Pid, {{X1, X2}, Func}),
  maps:put({X1, X2}, Func, FuncMap);

generate_function(quadratic, PointsList, FuncMap, Pid) ->
  {X1, Y1} = lists:nth(1, PointsList),
  {X2, Y2} = lists:nth(2, PointsList),
  {X3, Y3} = lists:nth(3, PointsList),
  A2 = ((Y3 - Y1) / ((X3 - X1) * (X3 - X2))) - ((Y2 - Y1) / ((X2 - X1) * (X3 - X2))),
  A1 = ((Y2 - Y1) / (X2 - X1)) - (A2 * (X2 + X1)),
  A0 = Y1 - A1 * X1 - A2 * X1 * X1,
  Func = fun(X) -> A0 + A1 * X + A2 * X * X end,
  pass_message(Pid, {{X1, X3}, Func}),
  maps:put({X1, X2}, Func, FuncMap).

transport_message(Pid, Message, cast) -> gen_server:cast(Pid, Message).

pass_message(0, _) -> ok;

pass_message(Pid, Message) -> points_generator:send_message(Pid, Message).