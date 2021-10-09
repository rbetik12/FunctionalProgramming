-module(fifth).
-author("vitaliy").

%% API
-export([recursion/1, fold/1, fold_division_1_to_20_checker/1]).

%% 2432902008176640000 is 2*3*4*5*6*7*8*9*10*11*12*13*14*15*16*17*18*19*20

print_answer(Ans) ->
  if
    Ans == 0 -> io:format("Answer wasn't found in range [100, 2432902008176640000] ~n");
    true -> io:format("Answer was found in range [100, 2432902008176640000]. The answer is: ~B~n", [Ans])
  end.

%% Naive recursion implementation %%

recursion(2432902008176640000) ->
  print_answer(0);

recursion(Number) ->
  Result = division_1_to_20_checker(Number, 2),
  if
    Result == 0 ->
      recursion(Number + 1);
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

%% Fold implementation %%

fold(Start) ->
  Ans = lists:all(fun fold_division_1_to_20_checker/1, lists:seq(Start, 232792561)),
  if
    Ans == false -> ok;
    true -> false
  end.

fold_division_1_to_20_checker(SeqItem) ->
  IsAnswer = lists:all(
    fun(Divider) ->
      if
        not (SeqItem rem Divider == 0) -> false;
        true -> true
      end
    end,
    lists:seq(2, 20)),

  if
    IsAnswer == true -> print_answer(SeqItem), false;
    true -> true
  end.




