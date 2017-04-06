-module(host).
-export([start/3, stop/1, init/2]).

start(Name, Domain, DNS) ->
    register(Name, spawn(host, init, [Domain, DNS])).

stop(Name) ->
    Name ! stop,
    unregister(Name).

init(Domain, DNS) ->
    DNS ! {register, Domain, {host, self()}},
    host().

host() ->
    receive
      {ping, From} ->
          io:format("ping from ~w~n", [From]),
          From ! pong,
          host();
      stop ->
          io:format("closing down~n", []),
          ok;
      Error ->
          io:format("strange message ~w~n", [Error]),
          host()
    end.
