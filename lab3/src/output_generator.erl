-module(output_generator).
-behaviour(gen_server).
-author("vitaliy").

-export([start_link/1]).
-export([init/1, handle_call/3]).

start_link(Mode) ->
  Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [Mode], [Mode]),
  Return.

init([Mode]) ->
  {ok, [Mode]}.

output_x_y(stdout, X, Y) -> io:format("~f;~f~n", [X, Y]);

output_x_y(test, _, _) -> ok.

handle_call({X, Y}, _, State) ->
  Mode = lists:nth(1, State),
  output_x_y(Mode, X, Y),
  {reply, ok, State};

handle_call(_, _, Delta) ->
  {reply, ok, Delta}.

