-module(testOpty).
-export([bench/2, waitForClientsRead/1, waitForClientsWrite/1, waitForClientsCommit/1]).


bench(N,M) ->
  Start = erlang:system_time(micro_seconds),
  server:start(M),
  run(N,M),

  % Falta implementar esto
  {Tread, Twrite, Tcommit} = waitForProcces(N),

  % FinishRead = erlang:system_time(micro_seconds),
  Finish = erlang:system_time(micro_seconds),

  TotaltimeRead = (Tread-Start)/1000000,
  TotaltimeWrite = (Twrite-Start)/1000000,
  TotaltimeCommit = (Tcommit-Start)/1000000,
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
    %fijate que, no parametrice las operaciones porque sino, iba a tener que ahcer 3 veces el recorrido de N.
    request(M,Read),
    request(M,Write),
    request(M,Commit),
    run(N-1,M)
  end.

%esto ya quedo medio bonito, si te fijas, parametrice tood lo posible pero aun asi repetimos algo de codig, podrias dejarl asi si queres
request(M,Op) ->
  This = self(),
  spawn(fun() ->
      Client = client:open(server),
      usesOperationInClient(Client,Op,M,This),
    end).

usesOperationInClient(Client,Read,M,This) ->
  client:read(Client,rand:uniform(M),This)

usesOperationInClient(Client,Write,M,This) ->
  client:write(Client,rand:uniform(M),rand:uniform(1000), This)

usesOperationInClient(Client,Commit,M,This) ->
  client:commit(HCommit,rand:uniform(M*10),This)




%Como ves, podria recibir 3 numeros y devolver una terna {time,time,time}, si no sale lo vemos desp
waitForProcces(R,W,C) ->
  if N == 0 ->
    ok;
  true ->
    receive
      {ok, Op, Handler} ->
        client:abort(Handler),
        waitForProcces(N-1);
      _ ->
        abort
    end
  end.
