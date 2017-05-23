-module(cast).
-export([start/0]).

start() ->
	register(casting, spawn(fun() -> cast([]) end)).


cast(Workers) ->
	receive
		{subscribe, List} ->
			cast(List);
		{msg, Message} ->
			io:format("soy cast y llego este mensaje: ~w~n", [Message]),
			sendMessage(Workers, Message),
			cast(Workers)
	end.


sendMessage(Workers, Message) ->
	io:format("estoy enviando este mensaje a todos: ~w~n", [Message]),
	lists:map(fun(Worker) -> Worker ! {msg, Message} end, Workers).
