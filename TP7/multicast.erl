-module(multicast).
-export().

start(Jitter,Master,Nodes) ->
  spawn(fun() -> server(Master,0,Nodes,[],queue:new(),Jitter) end).

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
      {Agreed, Queue2} = agreed(Seq, Update),%no estoy seguro
      deliver(Agreed, Master),
      Next2 = increment(Next, Seq),
      server(Master, Next2, Nodes, Cast, Queue2, Jitter);
  end.
