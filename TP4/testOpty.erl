-module(testOpty).
-export([bench/2, waitForClients/1]).


bench(N,M) ->
  Start = erlang:system_time(micro_seconds),

  server:start(M),
  client:open(server),
  run(N,M),
  waitForClients(N),
  Finish = erlang:system_time(micro_seconds),
  Totaltime = (Finish-Start)*1000000,
  io:format("  Tardo en segundos: ~w~n", [Totaltime]),
  io:format("  Cantidad de lecturas por segundo: ~w~n", [Totaltime/N]).



run(N,M) ->
  if N == 0 ->
    ok;
  true ->
    request(M),
    run(N-1,M)
  end.

request(M) ->
  spawn_link(fun() -> 
                client:read(1,rand:uniform(M),self()) 
            end).


waitForClients(N) ->
  if N == 0 ->
    server ! stop,
    client:abort();
  true -> 
    receive
      ok -> 
        io:format("  N:~w~n", [N]),
        waitForClients(N-1)
    end
  end.

