-module(function_generator).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3]).

-record(generator_state, {generator_type, points_list, func_map}).

start_link(GeneratorType) ->
  Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [GeneratorType], [GeneratorType]),
  Return.

init(GeneratorType) ->
  {ok, #generator_state{generator_type = GeneratorType, points_list = [], func_map = maps:new()}}.

handle_call({add_point, X, Y}, _, #generator_state{generator_type = Type, points_list = PointList, func_map = FuncMap}) ->
  case length(PointList) of
    2 ->
      {reply,
        ok,
        #generator_state{
          generator_type = linear,
          points_list = [{X, Y}],
          func_map = generate_function(Type, PointList, FuncMap)}};
    _ ->
      {reply,
        ok,
        #generator_state{
          generator_type = linear,
          points_list = lists:append(PointList, [{X, Y}]),
          func_map = FuncMap}}
  end;

handle_call({get_func_map}, _, #generator_state{generator_type = _, points_list = _, func_map = FuncMap} = State) ->
  {reply, FuncMap, State};

handle_call(Request, From, State) ->
  io:format("H: ~p~n", [Request]),
  {reply, ok, State}.

generate_function(linear, PointsList, FuncMap) ->
  {X1, Y1} = lists:nth(1, PointsList),
  {X2, Y2} = lists:nth(2, PointsList),
  A1 = (Y2 - Y1) / (X2 - X1),
  A0 = Y1 - A1 * X1,
  maps:put({X1, X2}, {linear, A0, A1}, FuncMap).