-module(pong).

-export([upPong/0]).

upPong() -> receive
    Pid ->  io:format("llega el pid: ~w~n", [Pid]),
            Pid ! {'HOLA','HOLA2'}
end.
