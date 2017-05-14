-module(testPDF).
-export([run/2]).
%report on your initial observations

run(Sleep, Jitter) ->
  Log = logger:start([john, paul, ringo, george]),
  A = workerPDF:start(john, Log, 13, Sleep, Jitter),
  B = workerPDF:start(paul, Log, 23, Sleep, Jitter),
  C = workerPDF:start(ringo, Log, 36, Sleep, Jitter),
  D = workerPDF:start(george, Log, 49, Sleep, Jitter),
  workerPDF:peers(A, [B, C, D]),
  workerPDF:peers(B, [A, C, D]),
  workerPDF:peers(C, [A, B, D]),
  workerPDF:peers(D, [A, B, C]),
  timer:sleep(5000),
  logger:stop(Log),
  workerPDF:stop(A),
  workerPDF:stop(B),
  workerPDF:stop(C),
  workerPDF:stop(D).
