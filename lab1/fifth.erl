-module(fifth).
-author("vitaliy").

%% API
-export([
  tail_recursion_start/1
%%  filter_start/1,
%%  map_start/1,
%%  recursion/1
]).

%% common code

print_answer(0) -> io:format("Answer wasn't found in range [100, 300000000] ~n");
print_answer(Ans) -> io:format("Answer was found in range [100, 300000000]. The answer is: ~B~n", [Ans]).

is_divided_without_rem_on_seq(Number, Start, Finish) ->
  lists:all(
    fun(Divider) ->
      if
        not (Number rem Divider == 0) -> false;
        true -> true
      end
    end,
    lists:seq(Start, Finish)).

%% Tail recursion implementation %%

tail_recursion_start(Number) -> tail_recursion(Number, false).

tail_recursion(300000000, Result) ->
  print_answer(0);

tail_recursion(Number, true) ->
  print_answer(Number - 1);

tail_recursion(Number, false) ->
  tail_recursion(Number + 1, is_divided_without_rem_on_seq(Number, 2, 20)).

%% Recursion implementation %%

%%recursion(300000000) ->
%%  0;
%%
%%recursion(Number) ->
%%  recursion(Number + 1),
%%  Result = division_1_to_20_checker(Number, 2),
%%  if
%%    Result == 0 -> ok;
%%    true -> print_answer(Result)
%%  end.
%%
%%%% Filter implementation %%
%%
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





