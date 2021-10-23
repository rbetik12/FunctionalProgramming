-module(fifth).
-author("vitaliy").

%% API
-export([
  tail_recursion_start/1,
  recursion_start/0
%%  filter_start/1,
%%  map_start/1,
]).

%% common code

print_answer(0) -> io:format("Answer wasn't found in range [100, 300000000] ~n");
print_answer(Ans) -> io:format("Answer was found in range [100, 300000000]. The answer is: ~B~n", [Ans]).

is_divided_result(0) -> true;
is_divided_result(Len) -> false.

is_divided_without_rem_on_seq(Number, Start, Finish) ->
  is_divided_result(length([X || X <- lists:seq(Start, Finish), Number rem X =/= 0])).

%% Tail recursion implementation %%

tail_recursion_start(Number) -> tail_recursion(Number, false).

tail_recursion(300000000, Result) -> print_answer(0);

tail_recursion(Number, true) -> print_answer(Number - 1);

tail_recursion(Number, false) -> tail_recursion(Number + 1, is_divided_without_rem_on_seq(Number, 2, 20)).

%% Recursion implementation %%

recursion_start() -> print_answer(recursion(1)).

recursion(300000000) -> 0;

recursion(Number) ->
  Result = recursion(Number + 1),
  if
    Result == 0 ->
      case is_divided_without_rem_on_seq(Number, 2, 20) of
        true -> Number;
        false -> Result
      end;
    true -> Result
  end.

%% Filter implementation %%

%%filter_start(Start) ->
%%  print_answer(lists:nth(
%%    1,
%%    lists:filter(
%%      fun is_divided_on_1_20/1,
%%      lists:filter(fun(Element) -> Element rem 2 == 0 end, lists:seq(Start, 300000000))
%%    )
%%  )).
%%
%%%% Map implementation %%
%%
%%map_start(Start) ->
%%  print_answer(
%%    erlang:element(
%%      2,
%%      lists:search(
%%        fun(Item) ->
%%          if
%%            not(Item == 0) -> true;
%%            true -> false
%%          end
%%        end,
%%        lists:map(
%%          fun(Item) ->
%%            IsAns = is_divided_on_1_20(Item),
%%            if
%%              IsAns == true -> Item;
%%              true -> 0
%%            end
%%          end,
%%          lists:seq(Start, 300000000)
%%        )
%%      ))).





