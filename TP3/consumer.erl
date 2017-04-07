-module(consumer).
-export([start/1, stop/0, init/1, recursiveFunction/1]).

start(producer) ->
    Consumer = spawn(fun() -> init(producer) end),
    register(consumer, Consumer).

stop() ->
    consumer ! stop.

init(producer) ->
    producer ! {hello, self()},
    recursiveFunction(0).

recursiveFunction(N) ->
    receive
        {ping, M} ->
            if
              M==N -> io:format("The number is: ~p",[M]);
              M>N  -> io:format(warning);
              true -> io:format("Error !")
            end,
              recursiveFunction(M+1);
         bye ->
              stop()
    end.
