-module(period).
-author("vitaliy").

%% API
-export([tail_recursion_start/0, recursion_start/0, fold_start/0, map_start/0, endless_list_start/0]).

-import(endless_list, []).

%% Tail recursion implementation %%

tail_recursion_start() ->
  tail_recursion(1, 0).

tail_recursion(1001, Max) ->
  io:format("Answer is ~B~n", [Max]);

tail_recursion(Number, Max) ->
  case string:length(period_generator(Number, 0, "", 1, maps:new())) of PeriodLen
    when PeriodLen > Max  -> tail_recursion(Number + 1, PeriodLen);
    _ -> tail_recursion(Number + 1, Max)
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
  case string:length(period_generator(Number, 0, "", 1, maps:new())) of PeriodLen
    when PeriodLen > NewMax -> PeriodLen;
    _ -> NewMax
  end.

%% Fold implementation %%

get_prime_list(Max) -> [X || X <- lists:seq(2, Max), is_prime(X)].

period_fold(N, PrimeList) ->
  lists:foldl(
    fun(_, MDigits) ->
      NewM = (lists:nth(1, MDigits) + 1) * 10 - 1,
      NewList = [X || X <- MDigits, NewM rem X =/= 0],
      [NewM | NewList -- [lists:nth(1, MDigits)]]
    end,
    [0 | PrimeList],
    lists:seq(2, N)
  ).

fold_start() ->
  io:format("~B~n", [lists:last(period_fold(981, get_prime_list(1001)))]).

%% Map implementation %%

map_start() -> map().

map() -> element(2, map(1, get_prime_list(1001), 0, 0)).

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

endless_list_start() -> endless_list_find_solution(1, 0, 0, maps:new()).

fill_map_loop(ListIter, Counter, M, Max, UsedPrimes) ->
  case endless_list:filter_next(ListIter, fun is_prime/1) of
    Error when Error == error -> exit("Endless list generator timed out!");
    NextPrime when NextPrime > 997 -> {Max, UsedPrimes};
    NextPrime when NextPrime =< 997 ->
      MaxUsedPrimesTuple = fill_map(NextPrime, Counter, M, Max, UsedPrimes),
      fill_map_loop(ListIter, Counter, M, element(1, MaxUsedPrimesTuple), element(2, MaxUsedPrimesTuple))
  end.

fill_map(NextPrime, Counter, M, Max, UsedPrimes) ->
  case maps:get(NextPrime, UsedPrimes, none) == none of IsNotInUsedPrimes
    when IsNotInUsedPrimes == true, M rem NextPrime == 0 ->
    NewUsedPrimes = maps:put(NextPrime, NextPrime, UsedPrimes),
    case Counter > Max of
      true -> {NextPrime, NewUsedPrimes};
      _ -> {Max, NewUsedPrimes}
    end;
    _ -> {Max, UsedPrimes}
  end.

endless_list_find_solution(1001, _, Max, _) -> Max;

endless_list_find_solution(Counter, M, Max, UsedPrimes) ->
  NewM = M * 10 + 9,
  ListIter = endless_list:create(fun(X) -> X + 1 end, 2),
  MaxUsedPrimesTuple = fill_map_loop(ListIter, Counter, NewM, Max, UsedPrimes),
  endless_list:delete(ListIter),
  endless_list_find_solution(Counter + 1, NewM, element(1, MaxUsedPrimesTuple), element(2, MaxUsedPrimesTuple)).
