-module(period).
-author("vitaliy").

%% API
-export([start_tail_recursion/0, start_recursion/0]).

%% Tail recursion implementation %%

start_tail_recursion() ->
  tail_recursion(1, 0).

tail_recursion(1001, Max) ->
  io:format("Answer is ~B~n", [Max]);

tail_recursion(Number, Max) ->
  PeriodLen = string:length(period_generator(Number, 0, "", 1, maps:new())),
  if
    PeriodLen > Max -> tail_recursion(Number + 1, PeriodLen);
    true -> tail_recursion(Number + 1, Max)
  end.

period_generator(N, Position, Period, Rem, FirstPos) ->
  PosFromMap = maps:get(Rem, FirstPos, none),
  if
    PosFromMap == none ->
      period_generator(
        N,
        Position + 1,
        Period ++ integer_to_list(Rem div N),
        (Rem rem N) * 10,
        maps:put(Rem, Position, FirstPos));
    true -> Period
  end.

%% Recursion implementation %%

start_recursion() ->
  recursion(1, 0).

recursion(1001, Max) ->
  Max;

recursion(Number, Max) ->
  NewMax = recursion(Number + 1, Max),
  PeriodLen = string:length(period_generator(Number, 0, "", 1, maps:new())),
  if
    PeriodLen > NewMax -> PeriodLen;
    true -> NewMax
  end.