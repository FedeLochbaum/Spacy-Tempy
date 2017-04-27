-module(testOpty).
-export([bench/1]).


bench(N) ->
  Start = erlang:system_time(micro_seconds),

  server:start(N),
  client:open(server),
  run(100),
  Finish = erlang:system_time(micro_seconds),
  Finish - Start.



run(N) ->
  if N == 0 ->
    ok;
  true ->
    request(),
    run(N-1)
  end.

request() ->
  client:read(1,2).
