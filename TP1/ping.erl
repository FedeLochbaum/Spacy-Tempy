-module(ping).

-export([send_ping/2]).

send_ping(Receptor, Node) -> {Receptor, Node} ! self(),
  receive
    A -> io:format("llega el mensaje: ~w~n", [A])
end.
