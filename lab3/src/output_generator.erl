-module(output_generator).
-behaviour(gen_server).
-author("vitaliy").

-export([start_link/1, send_message/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {mode}).

start_link(Mode) ->
  {_, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [Mode], []),
  Pid.

init([Mode]) -> {ok, #state{mode = Mode}}.

handle_cast({X, Y}, #state{mode = Mode}) ->
  io:format("~f;~f~n", [X, Y]),
  {noreply, #state{mode = Mode}}.

handle_call(_, _, _) -> throw("Call is not supported in output generator!").

send_message(Pid, Message) -> gen_server:cast(Pid, Message).
