-module(bench).
-export([start/5]).


start(N, Servers, Sleep, MaxRange, MaxTime) ->


  generateNodes({N, N}, Servers, Sleep, MaxRange, MaxTime).






generateNodes({0, N}, Servers, Sleep,_,MaxTime) ->
  receive
    after
      MaxTime ->
        stop(N),
        TotalServers = totalServers(Servers),
        io:format("Total servers: ~w~n", [TotalServers]),

        Weights = distributedServer:getWeight(TotalServers),
        io:format("Total Weights: ~w~n", [Weights]),

        MaxWeight = distributedServer:maxWeight(Weights),
        io:format("Max Weight: ~w~n", [MaxWeight]),

        TotalRequests = (MaxTime / Sleep) * N,
        io:format("TotalRequests: ~w~n", [TotalRequests])

        % printResult(Weights, MaxWeight, TotalServers, TotalRequests)
  end;

generateNodes({N, M}, Servers, Sleep, {Xmax, Ymax}, MaxTime) ->
  Name = list_to_atom("node" ++ integer_to_list(N)),
  Server = lists:nth(rand:uniform(length(Servers)), Servers),
  node:start(Name, Server, Sleep, {Xmax, Ymax}),
  generateNodes({N-1, M}, Servers, Sleep, {Xmax, Ymax}, MaxTime).


printResult(Weights, MaxWeight, TotalServers, TotalRequests) ->
  io:format("Total Weights: ~w~n", [Weights]),
  io:format("Max Weight: ~w~n", [MaxWeight]),
  io:format("Total servers: ~w~n", [TotalServers]),
  io:format("TotalRequests: ~w~n", [TotalRequests]).

stop(0) ->
  ok;%server:stop(server);

stop(N) ->
  Name = list_to_atom("node" ++ integer_to_list(N)),
  node:stop(Name),
  stop(N-1).


totalServers(Servers) ->
  Server = lists:nth(rand:uniform(length(Servers)), Servers),
  Server ! {peers, self()},
  receive
    {peers, Peers} ->
      io:format("Peers: ~w~n", [Peers]),
      Res = Peers;
    _ ->
      Res = 0,
      io:format("Error: ~w~n", [])
  end,
  Res.
