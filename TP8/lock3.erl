-module(lock3).
-export([start/1]).

start(XS) ->
  spawn(fun() -> init(XS) end).

init(Id) ->
  receive
    {peers, Peers} ->
      open(Id,Peers,0);
    stop ->
      ok
  end.

open(Id,Nodes,LamportTime) ->
  receive
    {take, Master} ->
      Refs = requests(Id,Nodes, LamportTime),
      wait(Nodes, Master, Refs, [], Id,LamportTime, LamportTime);
    {request, From, Ref, OtherId, Time} ->
      From ! {ok, Ref},
      open(Id,Nodes,max(LamportTime,Time) +1 );
    stop ->
      ok
  end.

requests(Id,Nodes, LamportTime) ->
  lists:map(fun(P) -> R = make_ref(), P ! {request, self(), R, Id, LamportTime}, R end, Nodes).

wait(Nodes, Master, [], Waiting, MyId, MaxTime, MyLamportTime) ->
  Master ! taken,
  held(Nodes, Waiting, MyId, MaxTime);

wait(Nodes, Master, Refs, Waiting, MyId, MaxTime, LamportTime) ->
  receive
    {request, From, Ref,IdFrom, Time} ->
      NewMaxTime = max(MaxTime,Time),
      if
        Time < LamportTime -> %IdFrom + 1  == MyId
          From ! {ok, Ref},
          wait(Nodes, Master, Refs, Waiting, MyId, NewMaxTime, LamportTime);
        Time == LamportTime ->
          if
            IdFrom < MyId ->
              From ! {ok, Ref},
              wait(Nodes, Master, Refs, Waiting, MyId, NewMaxTime, LamportTime);
            true ->
              wait(Nodes, Master, Refs, [{From, Ref}|Waiting], MyId, NewMaxTime, LamportTime)
          end;
        true ->
          wait(Nodes, Master, Refs, [{From, Ref}|Waiting], MyId, NewMaxTime, LamportTime)
      end;
    {ok, Ref} ->
      Refs2 = lists:delete(Ref, Refs),
      wait(Nodes, Master, Refs2, Waiting, MyId, MaxTime, LamportTime);
    release ->
      ok(Waiting),
      open(MyId,Nodes, MaxTime)
  end.

ok(Waiting) ->
  lists:foreach(fun({F,R}) -> F ! {ok, R} end, Waiting).

held(Nodes, Waiting, MyId, MaxTime) ->
  receive
    {request, From, Ref, OtherId, Time} ->
      held(Nodes, [{From, Ref}|Waiting], MyId, max(MaxTime,Time) +1);
    release ->
      ok(Waiting),
      open(MyId, Nodes, MaxTime)
  end.
