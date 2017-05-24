-module(cast).
-export([start/1]).

start(Jitter) ->
	spawn(fun() -> cast(Jitter) end).


cast(Jitter) ->
	receive
		{sendMessage, Workers, Message} ->
			sendMessage(Workers, Message,Jitter),
			cast(Jitter)
	end.


sendMessage(Workers, Message,Jitter) ->
	lists:map(fun (Worker) ->
							Worker ! {msg, Message},
							timer:sleep(Jitter)
						end, Workers).
