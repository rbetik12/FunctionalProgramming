-module(fifth).
-author("vitaliy").

%% API
-export([recursion_start/1, filter_start/1]).

%% 2432902008176640000 is 2*3*4*5*6*7*8*9*10*11*12*13*14*15*16*17*18*19*20

%% common code

print_answer(Ans) ->
  if
    Ans == 0 -> io:format("Answer wasn't found in range [100, 2432902008176640000] ~n");
    true -> io:format("Answer was found in range [100, 2432902008176640000]. The answer is: ~B~n", [Ans])
  end.

is_divided_on_1_20(Number) ->
  is_divided_without_rem_on_seq(Number, 2, 20).

is_divided_without_rem_on_seq(Number, Start, Finish) ->
  lists:all(
    fun(Divider) ->
      if
        not (Number rem Divider == 0) -> false;
        true -> true
      end
    end,
    lists:seq(Start, Finish)).

%% Naive recursion implementation %%

recursion_start(2432902008176640000) ->
  print_answer(0);

recursion_start(Number) ->
  Result = division_1_to_20_checker(Number, 2),
  if
    Result == 0 ->
      recursion_start(Number + 1);
    true ->
      print_answer(Result)
  end.

division_1_to_20_checker(Number, 21) ->
  Number;

division_1_to_20_checker(Number, Divider) ->
  if
    Number rem Divider == 0 -> division_1_to_20_checker(Number, Divider + 1);
    true -> 0
  end.

%% Filter implementation %%

filter_start(Start) ->
  print_answer(lists:nth(
    1,
    lists:filter(fun is_divided_on_1_20/1, lists:seq(Start, 300000000))
  )).





