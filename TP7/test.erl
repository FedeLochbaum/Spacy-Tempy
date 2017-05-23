-module(test).
-export([start/0,stop/0]).


start() ->
  cast:start(),
  worker:startManager(casting,600),
  worker:start(w1,manager,1000,casting),
  worker:start(w2,manager,1000,casting),
  worker:start(w3,manager,1000,casting),
  worker:start(w4,manager,1000,casting).


stop() ->
  worker:stop(w1),
  worker:stop(w2),
  worker:stop(w3),
  worker:stop(w4),
  worker:stop(manager),
  worker:stop(casting).
