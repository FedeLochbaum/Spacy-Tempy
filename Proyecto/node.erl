-module(node).
-export([start/3, stop/1]).

start(Name, Server, Sleep) ->
  register(Name, spawn_link(fun() -> init(Name, Server, Sleep) end)).

stop(Node) ->
  Node ! stop.

init(Name, Server, Sleep) ->
  X = rand:uniform(10),
  Y = rand:uniform(10),

  Server ! {subscribe, Name, {X, Y}},
  receive
    ok ->
      loop(Name, Server, Sleep, {X,Y});
    stop ->
      ok
  end.

loop(Name, Server, Sleep, {X,Y}) ->
  io:format("log: ~w ~p~n", [Name, {X,Y}]),
  XMove = rand:uniform(10),
  YMove = rand:uniform(10),
  Server ! {move, Name, {XMove, YMove}},
  receive
    stop ->
      ok
    after Sleep ->
      loop(Name, Server, Sleep, {X+XMove, Y+YMove})
  end.
