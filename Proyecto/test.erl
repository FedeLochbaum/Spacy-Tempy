-module(test).
-export([start/1,stop/1]).


start(N) ->
  server:start(server),
  Sleep = 1000,
  generateNodes(N,server,Sleep).


generateNodes(0,Server,Sleep) ->
  ok;

generateNodes(N,Server,Sleep) ->
  Name = list_to_atom("node" ++ integer_to_list(N)),
  node:start(Name,Server,Sleep),
  generateNodes(N-1,Server,Sleep).


stop(0) ->
  node:stop(server);

stop(N) ->
  Name = list_to_atom("node" ++ integer_to_list(N)),
  node:stop(Name),
  stop(N-1).
