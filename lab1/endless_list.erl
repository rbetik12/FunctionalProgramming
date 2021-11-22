-module(endless_list).
-author("vitaliy").

%% API
-export([create/2, internal_loop/2, next/1, filter_next/2, delete/1]).


%% Sender-side code %%
create(Func, Start) ->
  spawn(utils, endless_list, [Func, Start]).

internal_loop(Func, Next) ->
  receive
    {Pid} ->
      Pid ! Next,
      NewNext = Func(Next),
      internal_loop(Func, NewNext);
    finished -> ok
  end.

%% Receiver-side code %%

next(ListIter) ->
  ListIter ! {self()},
  receive
    Next -> Next
  end.

filter_next(ListIter, FilterFunc) ->
  Value = next(ListIter),
  case FilterFunc(Value) of
    true -> Value;
    _ -> filter_next(ListIter, FilterFunc)
  end.

delete(ListIter) ->
  ListIter ! finished.
