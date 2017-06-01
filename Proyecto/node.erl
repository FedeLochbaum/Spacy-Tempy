-module(node).
-export([start/3, stop/1,timeNow/0]).

start(Name, Server, Sleep) ->
  register(Name, spawn_link(fun() -> init(Name, Server, Sleep) end)).

stop(Node) ->
  Node ! stop.

init(Name, Server, Sleep) ->
  Time = timeNow(),
  X = rand:uniform(100),
  Y = rand:uniform(100),

  Server ! {subscribe, Name, {X, Y}, Time},
  receive
    ok ->
      loop(Name, Server, Sleep, {X,Y}, Time);
    stop ->
      ok
  end.

loop(Name, Server, Sleep, {X,Y}, Time) ->
  io:format("log: ~w ~w ~p~n", [Name, {X,Y}, Time]),
  XMove = rand:uniform(10),
  YMove = rand:uniform(10),
  Time2 = timeNow(),
  NewInstant = Time2 - Time,
  Server ! {move, Name, {XMove, YMove}, NewInstant},
  receive
    stop ->
      ok
    after Sleep ->
      loop(Name, Server, Sleep, {X+XMove, Y+YMove}, Time2)
  end.


timeNow() ->
  {H, M, S} = erlang:time(),
  H * 3600 + M * 60 + S.
