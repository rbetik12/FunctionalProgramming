-module(period).
-author("vitaliy").

%% API
-export([tail_recursion_start/0, start_recursion/0, start_fold/0, start_map/0]).

%% Tail recursion implementation %%

tail_recursion_start() ->
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
  case maps:get(Rem, FirstPos, none) of
    none ->
      period_generator(
        N,
        Position + 1,
        Period ++ integer_to_list(Rem div N),
        (Rem rem N) * 10,
        maps:put(Rem, Position, FirstPos));
    _ -> Period
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

%% Fold implementation %%

start_fold() ->
  fold().

fold() ->
  lists:foldl(
    fun(Number, Max) ->
      PeriodLen = string:length(period_generator(Number, 0, "", 1, maps:new())),
      if
        PeriodLen > Max -> PeriodLen;
        true -> Max
      end
    end,
    0,
    lists:seq(1, 1000)
  ).

%% Map implementation %%

start_map() ->
  map().

map() ->
  lists:max(
    lists:map(
      fun(Number) ->
        string:length(period_generator(Number, 0, "", 1, maps:new()))
      end,
      lists:seq(1, 1000)
    )
  ).
