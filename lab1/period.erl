-module(period).
-author("vitaliy").

%% API
-export([tail_recursion_start/0, recursion_start/0, fold_start/0, map_start/0, endless_list_start/0]).

-import(utils, []).

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
  element(2, map(1, get_prime_list(), 0, 0)).

map(1001, List, Max, M) -> {List, Max + 1};

map(Counter, List, Max, M) ->
  NewM = M * 10 + 9,
  ListAndMax = lists:mapfoldl(
    fun(Item, Max) ->
      case Item =/= 0 of
        Result when Result == true, NewM rem Item == 0 ->
          case Counter > Max of
            true -> {0, Counter};
            _ -> {0, Max}
          end;
        _ -> {Item, Max}
      end
    end,
    Max,
    List
  ),
  map(Counter + 1, element(1, ListAndMax), element(2, ListAndMax), NewM).

%% Endless list implementation %%

is_prime(Number) ->
  case Number of Number
    when Number =< 2 -> Number == 2;
    _ ->
      case Number rem 2 =/= 0 of
        true ->
          lists:all(
            fun(Item) ->
              Number rem Item =/= 0
            end,
            [X || X <- lists:seq(3, round(math:sqrt(Number))), X rem 2 =/= 0]
          );
        _ -> false
      end
  end.

endless_list_start() ->
  endless_list_find_solution(1, 0, 0, maps:new()).

fill_map_loop(ListIter, Counter, M, Max, UsedPrimes) ->
  NextPrime = utils:endless_list_filter_next(ListIter, fun is_prime/1),
  case NextPrime > 997 of
    true -> {Max, UsedPrimes};
    _ ->
      MaxUsedPrimesTuple = fill_map(NextPrime, Counter, M, Max, UsedPrimes),
      fill_map_loop(ListIter, Counter, M, element(1, MaxUsedPrimesTuple), element(2, MaxUsedPrimesTuple))
  end.

fill_map(NextPrime, Counter, M, Max, UsedPrimes) ->
  IsNotInUsedPrimes = maps:get(NextPrime, UsedPrimes, none) == none,
  case IsNotInUsedPrimes of IsNotInUsedPrimes
    when IsNotInUsedPrimes == true, M rem NextPrime == 0 ->
%%    io:format("    ~B ~B~n", [NextPrime, M]),
    NewUsedPrimes = maps:put(NextPrime, NextPrime, UsedPrimes),
    case Counter > Max of
      true -> {NextPrime, NewUsedPrimes};
      _ -> {Max, NewUsedPrimes}
    end;
    _ -> {Max, UsedPrimes}
  end.

endless_list_find_solution(1001, M, Max, UsedPrimes) -> Max;

endless_list_find_solution(Counter, M, Max, UsedPrimes) ->
  NewM = M * 10 + 9,
  ListIter = utils:create_endless_list(fun(X) -> X + 1 end, 2),
  MaxUsedPrimesTuple = fill_map_loop(ListIter, Counter, NewM, Max, UsedPrimes),
  utils:endless_list_delete(ListIter),
  endless_list_find_solution(Counter + 1, NewM, element(1, MaxUsedPrimesTuple), element(2, MaxUsedPrimesTuple)).
