-module(lab1).
-author("vitaliy").

%% API
-export([loop/1]).

loop(Counter) when Counter > 1000000000 ->
  Counter;
loop(Counter) ->
  Ans = internal_loop(Counter, 1, 1),
  if
    Ans == 0 ->
      loop(Counter + 1);
      0;
    true ->
      io:format("~B~n", [Ans])
  end.

internal_loop(Number, Result, Counter) when Counter > 20, (Result == 1) ->
  Number;
internal_loop(Number, Result, Counter) when Counter > 20, (Result == 0) ->
  0;
internal_loop(Number, Result, Counter) when Number rem Counter == 0 ->
  internal_loop(Number, 1, Counter + 1);
internal_loop(Number, Result, Counter) when not (Number rem Counter == 0) ->
  0.

