-module(cast).
-export([start/0]).

start() ->
	register(cast, spawn(fun() -> cast([]) end)).


cast(Workers) ->
	receive
		{subscribe, List} ->
			cast(List);
		{msg, Message} ->
			sendMessage(Workers, Message),
			cast(Workers)
	end.


sendMessage(Workers, Message) ->
	lists:map(fun(Worker) -> Worker ! {msg, Message} end, Workers).