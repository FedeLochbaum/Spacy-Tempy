-module(lock2).
-export([start/1]).

start(XS) ->
  spawn(fun() -> init(XS) end).

init(Id) ->
  receive
    {peers, Peers} ->
      open(Id,Peers);
    stop ->
      ok
  end.

open(Id,Nodes) ->
  receive
    {take, Master} ->
      Refs = requests(Id,Nodes),
      wait(Nodes, Master, Refs, [], Id, true);
    {request, From, Ref, OtherId} ->
      From ! {ok, Ref},
      open(Id,Nodes);
    stop ->
      ok
  end.

requests(Id,Nodes) ->
  lists:map(fun(P) -> R = make_ref(), P ! {request, self(), R, Id}, R end, Nodes).

wait(Nodes, Master, [], Waiting, MyId, CanSendOk) ->
  Master ! taken,
  held(Nodes, Waiting, MyId);

wait(Nodes, Master, Refs, Waiting, MyId, CanSendOk) ->
  receive
    {request, From, Ref,IdFrom} ->
      if
        IdFrom < MyId  andalso CanSendOk ->
          From ! {ok, Ref},
          wait(Nodes, Master, Refs, Waiting, MyId, false);
        true ->
          wait(Nodes, Master, Refs, [{From, Ref}|Waiting], MyId, CanSendOk)
      end;
    {ok, Ref} ->
      Refs2 = lists:delete(Ref, Refs),
      wait(Nodes, Master, Refs2, Waiting, MyId, CanSendOk);
    release ->
      ok(Waiting),
      open(MyId,Nodes)
  end.

ok(Waiting) ->
  lists:foreach(fun({F,R}) -> F ! {ok, R} end, Waiting).

held(Nodes, Waiting, MyId) ->
  receive
    {request, From, Ref, OtherId} ->
      held(Nodes, [{From, Ref}|Waiting], MyId);
    release ->
      ok(Waiting),
      open(MyId, Nodes)
  end.
