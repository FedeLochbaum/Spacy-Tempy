-module(multicast).
-export([server/6,start/2]).

start(Jitter,Master) ->
  spawn(fun() -> waiting(Master,0,maps:new(),maps:new(),Jitter) end).

waiting(Master,Next,Cast,Queue,Jitter) ->
  receive
    {nodes, Nodes} ->
      server(Master,Next,Nodes,Cast,Queue,Jitter)
  end.


server(Master,Next,Nodes,Cast,Queue,Jitter) ->
  receive
    {send, Msg} ->
      Ref = make_ref(),
      request(Ref, Nodes, Msg, self()),
      Cast2 = cast(Ref, length(Nodes), Cast),
      server(Master, Next, Nodes, Cast2, Queue, Jitter);
    {request, From, Ref, Msg} ->
      From ! {proposal, Ref, Next},
      Queue2 = insert(Next, Ref, Msg, Queue),
      Next2 = increment(Next),
      server(Master, Next2, Nodes, Cast, Queue2, Jitter);
    {proposal, Ref, Proposal} ->
      case proposal(Ref, Proposal, Cast) of
        {agreed, Seq, Cast2} ->
          agree(Ref,Seq,Nodes),
          server(Master, Next, Nodes, Cast2, Queue, Jitter);
        Cast2 ->
          server(Master, Next, Nodes, Cast2, Queue, Jitter)
      end;
    {agreed, Ref, Seq} ->
      Updated = update(Ref, Seq, Queue),
      {Agreed, Queue2} = agreed(Next, Updated),
      deliver(Agreed, Master),
      Next2 = increment(Next, Seq),
      server(Master, Next2, Nodes, Cast, Queue2, Jitter)
  end.


request(Ref, Nodes, Msg, From) ->
  lists:map(fun (Node) ->
							Node ! {request, From, Ref, Msg}
						end, Nodes).

cast(Ref, Size, Cast) ->
  maps:put(Ref, {Size, 0}, Cast).

insert(Next, Ref, Msg, Queue) ->
  maps:put(Next, {Ref,Msg}, Queue). % [{Next, Ref, Msg} | Queue].

increment(Next) ->
  Next+1.

proposal(Ref, Proposal, Cast) ->
  case maps:find(Ref, Cast) of
    {ok, {L, Sofar} } ->
      Max = max(Sofar,Proposal),
      case L == 1 of
        true ->
          Cast2 = maps:remove(Ref,Cast),
          {agreed, Max, Cast2};
        false ->
          maps:put(Ref,{L-1,Max},Cast)
      end;
    error ->
      ok
  end.

agree(Ref,Seq,Nodes) ->
  lists:map(fun (Node) ->
							Node ! {agreed, Ref, Seq}
						end, Nodes).

update(NewRef, Seq, Queue) ->
  Fun = fun(K,{Ref,M}) ->
          case NewRef == Ref of
            true ->
              Max = max(K,Seq),
              maps:remove(K,Queue),
              maps:put(Max,{Ref,M},Queue);
            false ->
              maps:put(K,{Ref,M},Queue)
          end
        end,
  maps:map(Fun,Queue).

agreed(Next, Update) ->
  FunAgreeds = fun(K,_) ->
          K =< Next
        end,
  FunUpdate = fun(K,_) ->
          K > Next
        end,

  Agreeds = maps:filter(FunAgreeds,Update),
  Update2 = maps:filter(FunUpdate,Update),
  {maps:to_list(Agreeds),Update2}.

increment(Next, Seq) ->
  case Next < Seq of
    true -> max(Next,Seq) + 1;
    false -> Next
  end.

deliver(Agreed, Master) ->
  lists:map(fun ({Msg,_}) ->
							Master ! {msg, Msg}
						end, Agreed).
