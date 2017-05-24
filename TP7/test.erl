-module(test).
-export([start/0,stop/0]).


start() ->
  cast:start(),
  worker:start(w1,1000,casting),
  worker:start(w2,1000,casting),
  worker:start(w3,1000,casting),
  worker:start(w4,1000,casting),
  worker:peers({w1,w2,w3,w4}, casting).

stop() ->
  worker:stop(w1),
  worker:stop(w2),
  worker:stop(w3),
  worker:stop(w4),
  worker:stop(casting).
