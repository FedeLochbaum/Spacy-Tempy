-module(consumer).
-export([start/1, stop/0, init/1, consumer/2]).

start(Producer) ->
    Consumer = spawn(fun() -> init(Producer) end),
    register(consumer, Consumer).

stop() ->
    consumer ! stop.

init(Producer) ->
    Monitor = monitor(process, Producer),
    Producer ! {hello, self()},
    consumer(0, Monitor).

consumer(N,Monitor) ->
    receive
        {ping, M} ->
            if
              M==N -> io:format("The number is: ~p",[M]);
              M>N  -> io:format(warning);
              true -> io:format("Error !")
            end,
              consumer(M+1,Monitor);
         bye ->
              stop();
        {'DOWN', Monitor, process, Object, Info} ->
              io:format("~w died; ~w~n", [Object, Info]),
              consumer(N, Monitor)
    end.
