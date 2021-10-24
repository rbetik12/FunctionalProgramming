-module(period).
-author("vitaliy").

%% API
-export([tail_recursion_start/0, recursion_start/0, fold_start/0, map_start/0]).

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

recursion_start() ->
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

get_prime_list() ->
  [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997].

period_fold(N, PrimeList) ->
  lists:foldl(
    fun(Num, MDigits) ->
      NewM = (lists:nth(1, MDigits) + 1) * 10 - 1,
      NewList = [X || X <- MDigits, NewM rem X =/= 0],
      [NewM | NewList -- [lists:nth(1, MDigits)]]
    end,
    [0 | PrimeList],
    lists:seq(2, N)
  ).

fold_start() ->
  io:format("~B~n", [lists:last(period_fold(981, get_prime_list()))]).

%% Map implementation %%

map_start() ->
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
