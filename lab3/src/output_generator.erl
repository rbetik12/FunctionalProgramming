-module(output_generator).
-behaviour(gen_server).
-author("vitaliy").

-export([start_link/0]).
-export([init/1, handle_call/3]).

start_link() ->
  Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
  Return.

init([]) ->
  {ok, []}.

output_x_y(stdout, X, Y) -> io:format("~f;~f~n", [X, Y]).

handle_call({X, Y}, _, State) ->
  output_x_y(stdout, X, Y),
  {reply, ok, State};

handle_call(_, _, Delta) ->
  {reply, ok, Delta}.

