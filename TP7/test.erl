-module(test).
-export([start/0,stop/0]).


start() ->
  worker:startManager(2000),
  Sleep = 1000,
  Jitter = 100,
  worker:start(w1,manager,Sleep,Jitter),
  worker:start(w2,manager,Sleep,Jitter),
  worker:start(w3,manager,Sleep,Jitter),
  worker:start(w4,manager,Sleep,Jitter).


stop() ->
  worker:stop(w1),
  worker:stop(w2),
  worker:stop(w3),
  worker:stop(w4),
  worker:stop(manager).
