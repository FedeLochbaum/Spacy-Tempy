-module(multicast).
-export([server/6,start/3]).

start(Jitter,Master,Nodes) ->
  spawn(fun() -> server(Master,0,Nodes,maps:new(),maps:new(),Jitter) end).

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
      {Agreed, Queue2} = agreed(Seq, Updated),%no estoy seguro
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
  maps:put(Next,{Ref,Msg}). % [{Next, Ref, Msg} | Queue].

increment(Next) ->
  Next+1.

proposal(Ref, Proposal, Cast) ->
  case maps:find(Ref, Cast) of
    {ok, {L, Sofar} } ->
      Max = max(Sofar,Proposal),
      if
        L == 1 ->
          Cast2 = maps:remove(Ref,Cast),
          {agreed, Max, Cast2};
        false ->
          maps:put(Ref,{L-1,Max})
      end
  end.

agree(Ref,Seq,Nodes) ->
  lists:map(fun (Node) ->
							Node ! {agreed, Ref, Seq}
						end, Nodes).

update(Ref, Seq, Queue) ->
  lists:map(fun ({Next,Ref,Msg}) ->
              Max = max(Next,Seq),
							{Max,Ref,Msg}
						end, Queue).

agreed(Seq, Update) ->
  lists:filter(fun {Next,Ref,Msg} ->
							Node ! {agreed, Ref, Seq}
						end, Update).
