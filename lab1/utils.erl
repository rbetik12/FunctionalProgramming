-module(utils).
-author("vitaliy").

%% API
-export([create_endless_list/2, endless_list/2, endless_list_next/1, endless_list_filter_next/2, endless_list_delete/1]).


%% Sender-side code %%
create_endless_list(Func, Start) ->
  spawn(utils, endless_list, [Func, Start]).

endless_list(Func, Next) ->
  receive
    {Pid} ->
      Pid ! Next,
      NewNext = Func(Next),
      endless_list(Func, NewNext);
    finished -> ok
  end.

%% Receiver-side code %%

endless_list_next(ListIter) ->
  ListIter ! {self()},
  receive
    Next -> Next
  end.

endless_list_filter_next(ListIter, FilterFunc) ->
  Value = endless_list_next(ListIter),
  case FilterFunc(Value) of
    true -> Value;
    _ -> endless_list_filter_next(ListIter, FilterFunc)
  end.

endless_list_delete(ListIter) ->
  ListIter ! finished.