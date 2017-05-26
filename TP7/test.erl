-module(test).
-export([start/0,stop/0]).


start() ->
  worker:startManager(2000),
  Sleep = 1000,
  Jitter = 100,
  worker:start(w1,manager,Sleep,Jitter),
  worker:start(w2,manager,Sleep,Jitter),
  worker:start(w3,manager,Sleep,Jitter),
  worker:start(w4,manager,Sleep,Jitter),
  worker:start(w5,manager,Sleep,Jitter),
  worker:start(w6,manager,Sleep,Jitter),
  worker:start(w7,manager,Sleep,Jitter),
  worker:start(w8,manager,Sleep,Jitter),
  worker:start(w9,manager,Sleep,Jitter),
  worker:start(w10,manager,Sleep,Jitter),
  worker:start(w11,manager,Sleep,Jitter).


stop() ->
  worker:stop(w1),
  worker:stop(w2),
  worker:stop(w3),
  worker:stop(w4),
  worker:stop(w5),
  worker:stop(w6),
  worker:stop(w7),
  worker:stop(w8),
  worker:stop(w9),
  worker:stop(w10),
  worker:stop(w11),
  worker:stop(manager).
