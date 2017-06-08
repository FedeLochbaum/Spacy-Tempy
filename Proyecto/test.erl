-module(test).
-export([start/1,stop/1]).


start(N) ->
  server:start(server),
  Sleep = 5000,
  generateNodes(N,server,Sleep),
  generateQueries(server,N, Sleep),
  stop(N).


generateQueries(_,0,_) ->
  ok;

generateQueries(Server,N,TimeSleep) ->
  Time = rand:uniform(TimeSleep),
  timer:sleep(Time),
  Name = list_to_atom("node" ++ integer_to_list(N)),
  Query = rand:uniform(4),
  sendQuery(Server,Name, Query),
  receive
    {reply, Reply} ->
      io:format("Reply : ~w~n", [Reply]),
      generateQueries(Server, N-1, TimeSleep)
  end.


sendQuery(Server, _, 1) ->
  X = rand:uniform(100),
  Y = rand:uniform(100),
  Instant = rand:uniform(server:timeNow()),
  Server ! {timelapse, {X, Y}, Instant, self()};

sendQuery(Server, _, 2) ->
  X  = rand:uniform(100),
  Y  = rand:uniform(100),
  Tk = rand:uniform(server:timeNow()),
  Ti = rand:uniform(server:timeNow()),
  Server ! {interval, {X, Y}, {min(Ti,Tk),max(Ti,Tk)}, self()};

sendQuery(Server, _, 3) ->
  X1 = rand:uniform(100),
  Y1 = rand:uniform(100),
  X2 = rand:uniform(100),
  Y2 = rand:uniform(100),
  Server ! {event, {min(X1,X2),min(Y1,Y2)}, {max(X1,X2),max(Y1,Y2)}, self()};

sendQuery(Server, Name, 4) ->
  Server ! {track, Name, self()}.


generateNodes(0,_,_) ->
  ok;

generateNodes(N,Server,Sleep) ->
  Name = list_to_atom("node" ++ integer_to_list(N)),
  node:start(Name,Server,Sleep),
  generateNodes(N-1,Server,Sleep).


stop(0) ->
  ok;%server:stop(server);

stop(N) ->
  Name = list_to_atom("node" ++ integer_to_list(N)),
  node:stop(Name),
  stop(N-1).
