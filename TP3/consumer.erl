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
            case M==N of
                true ->
                    io:format("M: ~p",M),
                    recursiveFunction(N+1);
                false ->
                    case M>N of
                        true ->
                            io:format(warning),
                            recursiveFunction(M+1);
                        false ->
                            io:format("Error !")
                    end
              end;
         bye ->
              stop()
    end.
