-module(testOpty).
-export([bench/2, waitForClientsRead/1, waitForClientsWrite/1, waitForClientsCommit/1]).


bench(N,M) ->
  Start = erlang:system_time(micro_seconds),
  server:start(M),
  run(N,M),
  waitForClientsRead(N),
  FinishRead = erlang:system_time(micro_seconds),
  waitForClientsWrite(N),
  FinishWrite = erlang:system_time(micro_seconds),
  waitForClientsCommit(N),
  FinishCommit = erlang:system_time(micro_seconds),
  Finish = FinishCommit,

  TotaltimeRead = (FinishRead-Start)/1000000,
  TotaltimeWrite = (FinishWrite-Start)/1000000,
  TotaltimeCommit = (FinishCommit-Start)/1000000,
  Totaltime = (Finish-Start)/1000000,
  io:format("  Cantidad de clientes: ~w~n", [N]),
  io:format("  Cantidad de entradas: ~w~n", [M]),
  io:format("  Promedio de lecturas por segundo: ~w~n", [TotaltimeRead/N]),
  io:format("  Promedio de escrituras por segundo: ~w~n", [TotaltimeWrite/N]),
  io:format("  Promedio de commit por segundo: ~w~n", [TotaltimeCommit/N]),
  io:format("  Tardo en segundos: ~w~n", [Totaltime]).



run(N,M) ->
  if N == 0 ->
    ok;
  true ->
    request(M),
    run(N-1,M)
  end.

request(M) ->
  This = self(),
  spawn(fun() ->
                HRead = client:open(server),
                client:read(HRead,rand:uniform(M),This),
                HWrite = client:open(server),
                client:write(HWrite,rand:uniform(M),rand:uniform(1000), This),
                HCommit = client:open(server),
                client:commit(HCommit,rand:uniform(M*10),This)
            end).


waitForClientsRead(N) ->
  if N == 0 ->
    io:format("  Termino Read\n");
  true ->
    receive
      {ok, Handler} ->
        client:abort(Handler),
        waitForClientsRead(N-1);
      _ ->
        abort
    end
  end.

waitForClientsWrite(N) ->
  if N == 0 ->
    io:format("  Termino write\n");
  true ->
    receive
      {ok, Handler} ->
        client:abort(Handler),
        waitForClientsWrite(N-1);
      _ ->
        abort
    end
  end.

waitForClientsCommit(N) ->
  if N == 0 ->
    io:format("  Termino commit\n\n"),
    server ! stop;
  true ->
    receive
      {ok, Handler} ->
        client:abort(Handler),
        waitForClientsCommit(N-1);
      _ ->
        abort
    end
  end.  