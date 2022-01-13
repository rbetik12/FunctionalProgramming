-module(lab3).

-behaviour(application).

-export([start/2, stop/1]).

get_x_y() ->
  Line = io:get_line(""),
  case Line of
    eof -> eof;
    _ ->
      StrippedLine = string:strip(string:strip(Line, both, 13), both, 10),
      {_, [X, Y], _} = io_lib:fread("~f~f", StrippedLine),
      {X, Y}
  end.

input_stream(FuncGenPid) ->
  case get_x_y() of
    eof -> eof;
    {X, Y} ->
      function_generator:add_point(FuncGenPid, X, Y),
      input_stream(FuncGenPid)
  end.

start(Mode, Delta) ->
  OutputGenPid = output_generator:start_link(stdout),
  PointsGenPid = points_generator:start_link(Delta, OutputGenPid),
  FuncGenPid = function_generator:start_link(Mode, PointsGenPid),
  input_stream(FuncGenPid).

stop(_State) -> ok.
