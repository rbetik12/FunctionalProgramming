-module(endless_list).
-author("vitaliy").

%% API
-export([create/2, internal_loop/2, next/1, filter_next/2, delete/1]).

create(Func, Start) ->
  spawn(endless_list, internal_loop, [Func, Start]).

internal_loop(Func, Next) ->
  receive
    {Pid} ->
      Pid ! Next,
      NewNext = Func(Next),
      internal_loop(Func, NewNext);
    finished -> ok
  end.

next(ListIter) ->
  ListIter ! {self()},
  receive
    Next -> Next
  after 10000 -> error
  end.

filter_next(ListIter, FilterFunc) ->
  case next(ListIter) of
    Next when Next == error -> error;
    Next when Next =/= error ->
      case FilterFunc(Next) of
        true -> Next;
        _ -> filter_next(ListIter, FilterFunc)
      end
  end.

delete(ListIter) ->
  ListIter ! finished.
