-module(test).
-export([start/1,stop/1]).


start(N) ->
  distributedServer:start(s1, {0,0}, {50,50}),
  distributedServer:start(s2, {50,50}, {100,100}),
  distributedServer:start(s3, {100,100}, {150,150}),
  distributedServer:start(s4, {150,150}, {200,200}),
  s1 ! {peers, [s2,s3,s4], s2},
  s2 ! {peers, [s3,s4,s1], s3},
  s3 ! {peers, [s4,s1,s2], s4},
  s4 ! {peers, [s1,s2,s3], s1},

  Sleep = 500,
  Servers = [s1,s2,s3,s4],
  S = lists:nth(rand:uniform(length(Servers)), Servers).
  % generateNodes(N,S,Sleep).
  % generateQueries(server,N, Sleep),
  % stop(N).


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
  Tk = rand:uniform(server:timeNow()),
  Ti = rand:uniform(server:timeNow()),
  Server ! {track, Name, {min(Ti,Tk),max(Ti,Tk)}, self()}.


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
