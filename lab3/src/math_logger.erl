-module(math_logger).
-behaviour(gen_server).
-author("vitaliy").

-export([start_link/0, log_point/3]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
  {_, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
  Pid.

log_point(Pid, X, Y) -> transport_message(Pid, {X, Y}).

init(_) -> {ok, []}.

handle_cast({X, Y}, _) ->
  io:format("~f;~f~n", [X, Y]),
  {noreply, []}.

handle_call(_, _, _) -> throw("logger doesn't support gen_server casts").

transport_message(Pid, Message) -> gen_server:cast(Pid, Message).
