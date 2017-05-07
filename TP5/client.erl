-module(client).
-export([test/2]).

test(Host, Res) ->
    io:format("looking up ~w~n", [Host]),
    Res ! {request, self(), Host},
    receive
        {reply, {host, Pid}} ->
            io:format("sending ping ...", []),
            Pid ! {ping, self()},
            receive
              pong ->
                  io:format("pong reply~n")
              after 1000 ->
                  io:format("no reply~n")
            end;
        {reply, unknown} ->
            io:format("unknown host~n", []),
            ok;
        Strange ->
            io:format("strange reply from resolver: ~w~n", [Strange]),
            ok
        after 1000 ->
            io:format("no reply from resolver~n", []),
            ok
    end.
