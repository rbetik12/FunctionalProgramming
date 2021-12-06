-module(function_generator_tests).
-author("vitaliy").

-include_lib("eunit/include/eunit.hrl").

linear_function_generator_test() ->
  function_generator:start_link(linear),
  points_generator:start_link(0.25),
  gen_server:call(function_generator, {add_point, -2.5, 0.0}),
  gen_server:call(function_generator, {add_point, -1.0, 3.0}),
  gen_server:call(function_generator, {add_point, -0.0, 5.0}),
  gen_server:call(function_generator, {add_point, 1.0, 7.0}),
  gen_server:call(function_generator, {add_point, 2.5, 10.0}),
  FuncMap = gen_server:call(function_generator, {get_func_map}),
  Func = maps:get({-2.5, -1.0}, FuncMap),
  ?assert(Func(8) - 21.0 < 0.0000001),
  ?assert(Func(-10) + 15 < 0.0000001).