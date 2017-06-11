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
      wait(Nodes, Master, Refs, [], Id);
    {request, From, Ref, OtherId} ->
      From ! {ok, Ref},
      open(Id,Nodes);
    stop ->
      ok
  end.

requests(Id,Nodes) ->
  lists:map(fun(P) -> R = make_ref(), P ! {request, self(), R, Id}, R end, Nodes).

wait(Nodes, Master, [], Waiting, MyId) ->
  Master ! taken,
  held(Nodes, Waiting, MyId);

wait(Nodes, Master, Refs, Waiting, MyId) ->
  receive
    {request, From, Ref,IdFrom} ->
      if
        IdFrom + 1  == MyId -> %IdFrom + 1  == MyId
          From ! {ok, Ref},
          Ref2 = requests(MyId,[From]),
          Nrefs = lists:append(Ref2, Refs),
          wait(Nodes, Master, Nrefs, Waiting, MyId);
        true ->
          wait(Nodes, Master, Refs, [{From, Ref}|Waiting], MyId)
      end;
    {ok, Ref} ->
      Refs2 = lists:delete(Ref, Refs),
      wait(Nodes, Master, Refs2, Waiting, MyId);
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
