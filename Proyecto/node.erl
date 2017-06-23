-module(node).
-export([start/4, stop/1]).

start(Name, Server, Sleep, {Xmax, Ymax}) ->
  register(Name, spawn_link(fun() -> init(Name, Server, Sleep, {Xmax, Ymax}) end)).

stop(Node) ->
  Node ! stop.

init(Name, Server, Sleep, {Xmax, Ymax}) ->
  X = rand:uniform(10),
  Y = rand:uniform(10),


  io:format("subscibe : ~w ~p~n", [Name, {X,Y}]),
  Server ! {subscribe, Name, {X, Y}},
  receive
    ok ->
      loop(Name, Server, Sleep, {X,Y}, {Xmax, Ymax});
    stop ->
      ok
  end.

loop(Name, Server, Sleep, {X,Y}, {Xmax, Ymax}) ->
  XMove = rand:uniform(10),
  YMove = rand:uniform(10),
  MinX = min(X+XMove, Xmax),
  MinY = min(Y+YMove, Ymax),
  SleepR = rand:uniform(Sleep),
  io:format("~w Send move to:  ~p~n", [Name, {MinX, MinY}]),
  Server ! {move, Name, {MinX, MinY}},
  receive
    stop ->
      ok
    after SleepR ->

      loop(Name, Server, Sleep, {MinX, MinY}, {Xmax, Ymax})
  end.
