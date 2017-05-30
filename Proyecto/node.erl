-module(node).
-export([start/3, stop/1]).

start(Name, Server, Sleep) ->
  register(Name, spawn_link(fun() -> init(Name, Server, Sleep) end)).

stop(Node) ->
  Node ! stop.

init(Name, Server, Sleep) ->
  {H, M, S} = erlang:time(),
  Instant = H * 3600 + M * 60 + S,
  X = rand:uniform(100),
  Y = rand:uniform(100),

  Server ! {subscribe, self(), {X, Y}, Instant},
  receive
    ok ->
      loop(Name, Server, Sleep, {X,Y}, Instant);
    stop ->
      ok
  end.

loop(Name, Server, Sleep, {X,Y}, Instant) ->
  io:format("log: ~w ~w ~p~n", [Name, {X,Y}, Instant]),
  receive
    stop ->
      ok
  end.
