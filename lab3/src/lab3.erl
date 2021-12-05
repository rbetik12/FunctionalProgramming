-module(lab3).

-behaviour(application).

-export([start/2, stop/1]).

get_x_y() ->
  Line = io:get_line("Enter new x y:"),
  case Line of
    eof -> {eof, {0.0, 0.0}};
    _ -> StrippedLine = string:strip(string:strip(Line, both, 13), both, 10),

      {ok, [X, Y], _} = io_lib:fread("~f~f", StrippedLine),
      {ok, {X, Y}}
  end.

input_stream() ->
  {Status, {X, Y}} = get_x_y(),
  io:format("~p~n", [Status]),
  case Status of
    eof -> Status;
    _ ->
      gen_server:call(function_generator, {add_point, X, Y}),
      io:format("Map: ~p~n", [gen_server:call(function_generator, {get_func_map})]),
      input_stream()
  end.

start(Mode, Freq) ->
  function_generator:start_link(Mode),
  input_stream().

stop(_State) ->
  ok.
