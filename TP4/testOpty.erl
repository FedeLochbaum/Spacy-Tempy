-module(testOpty).
-export([bench/2]).


bench(N,M) ->
  Start = erlang:system_time(micro_seconds),
  server:start(M),

  {CantRead, CantWrites, CantCommits} = run(N,M),
  {Tread, Twrite, Tcommit}  = waitForTests(0,0,0),

  Finish = erlang:system_time(micro_seconds),

  TotaltimeRead = (Tread-Start)/1000000,
  TotaltimeWrite = (Twrite-Start)/1000000,
  TotaltimeCommit = (Tcommit-Start)/1000000,
  Totaltime = (Finish-Start)/1000000,

  io:format("  Cantidad de clientes lectores: ~w~n", [CantRead]),
  io:format("  Cantidad de clientes escritores: ~w~n", [CantWrites]),
  io:format("  Cantidad de clientes commiteadores: ~w~n", [CantCommits]),
  io:format("  Cantidad total de clientes: ~w~n", [CantRead+CantWrites+CantCommits]),
  io:format("  Cantidad de entradas: ~w~n", [M]),
  io:format("  Promedio de lecturas por segundo: ~w~n", [TotaltimeRead/CantRead]),
  io:format("  Promedio de escrituras por segundo: ~w~n", [TotaltimeWrite/CantWrites]),
  io:format("  Promedio de commits por segundo: ~w~n", [TotaltimeCommit/CantCommits]),
  io:format("  Tardo en segundos: ~w~n", [Totaltime]).



run(N,M) ->
  This = self(),
  CReads = rand:uniform(N),
  CWrites = rand:uniform(N),
  CCommit = rand:uniform(N),
  spawn(fun() -> runOperation(CReads,M, read,This) end),
  spawn(fun() -> runOperation(CWrites,M, write,This) end),
  spawn(fun() -> runOperation(CCommit,M, commit,This) end),
  {CReads,CWrites,CCommit}.



runOperation(N,M,Op,Father) ->
  if N == 0 ->
    waitForProcces(N),
    Father ! {erlang:system_time(micro_seconds),Op};
  true ->
    request(M,Op),
    runOperation(N-1,M,Op,Father)
  end.

waitForTests(R,W,C) ->
  if ((R /= 0) and (W /= 0) and (C /= 0)) ->
    server ! stop,
    {R,W,C};
  true ->
    receive
      {T, read} ->
        waitForTests(T,W,C);

      {T, write} ->
        waitForTests(R,T,C);

      {T, commit} ->
        waitForTests(R,W,T);

      _ ->
        abort
    end
  end.


request(M,Op) ->
  This = self(),
  spawn(fun() ->
      Client = client:open(server),
      usesOperationInClient(Client,Op,M,This)
    end).

usesOperationInClient(Client,read,M,This) ->
  client:read(Client,rand:uniform(M),This);

usesOperationInClient(Client,write,M,This) ->
  client:write(Client,rand:uniform(M),rand:uniform(1000), This);

usesOperationInClient(Client,commit,M,This) ->
  client:commit(Client,rand:uniform(M*10),This).


waitForProcces(N) ->
  if N == 0 ->
    ok;
  true ->
  receive
    {ok, Handler} ->
      client:abort(Handler),
      waitForProcces(N-1);
    _ ->
      abort
  end
end.
