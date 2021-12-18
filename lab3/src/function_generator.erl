-module(function_generator).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_cast/2, send_message/2, handle_call/3]).

-record(generator_state, {generator_type, points_gen_pid, points_list = [], func_map = maps:new()}).

start_link(GeneratorType, PointsGenPid) ->
  {_, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [GeneratorType, PointsGenPid], []),
  Pid.

init([GeneratorType, PointsGenPid]) ->
  {ok, #generator_state{generator_type = GeneratorType, points_gen_pid = PointsGenPid}}.

handle_cast({add_point, X, Y},
    #generator_state{generator_type = linear,
                      points_gen_pid = Pid,
                      points_list = PointList,
                      func_map = FuncMap}) ->
  case length(PointList) of
    2 ->
      {noreply,
        #generator_state{
          generator_type = linear,
          points_list = [lists:nth(2, PointList), {X, Y}],
          func_map = generate_function(linear, PointList, FuncMap, Pid),
          points_gen_pid = Pid}};
    _ ->
      {noreply,
        #generator_state{
          generator_type = linear,
          points_list = lists:append(PointList, [{X, Y}]),
          func_map = FuncMap,
          points_gen_pid = Pid}}
  end;

handle_cast({get_func_map}, #generator_state{func_map = FuncMap} = State) -> {reply, FuncMap, State}.

send_message(Pid, Data) -> gen_server:cast(Pid, Data).

handle_call(_, _, _) -> throw("Call is not supported in function generator!").

generate_function(linear, PointsList, FuncMap, Pid) ->
  {X1, Y1} = lists:nth(1, PointsList),
  {X2, Y2} = lists:nth(2, PointsList),
  A1 = (Y2 - Y1) / (X2 - X1),
  A0 = Y1 - A1 * X1,
  Func = fun (X) -> A0 + A1 * X end,
  points_generator:send_message(Pid, {{X1, X2}, Func}),
  maps:put({X1, X2}, Func, FuncMap).