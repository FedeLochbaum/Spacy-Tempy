-module(distributedServer).
-export([start/4,stop/1,timeNow/0, addServer/3]).
-import(rstar, [rstar/1]).

start(Name, {InitialX, InitialY}, {FinalX, FinalY}, MaxRange) ->
  register(Name, spawn(fun() -> init(Name, {InitialX, InitialY}, {FinalX, FinalY}, MaxRange) end)).


init(Name, {InitialX, InitialY}, {FinalX, FinalY}, MaxRange) ->
  receive
    {peers, Peers, Next} ->
      server(Name, Peers, Next, i3RTree:new(), {InitialX, InitialY}, {FinalX, FinalY}, MaxRange);
    stop ->
      ok
  end.

startNewServer(Name, {MinX, MinY}, {MaxX, MaxY}, NewRtree, MaxRange, Peers, Sig) ->
  io:format(" Start New Serve : ~w ~w ~w ~w ~n", [Name, {MinX, MinY}, {MaxX, MaxY}, Sig]),
  register(Name, spawn(fun() -> server(Name, Peers, Sig, NewRtree,  {MinX, MinY}, {MaxX, MaxY}, MaxRange) end)).

addServer(Name, Peers, MaxRange) ->
  spawn(
        fun() ->
          Replies = getWeight(Peers),
          {S,Sig} = maxWeight(Replies),
          io:format("Selected Servers are ~w~n", [{S,Sig}]),
          % sendOk(lists:subtract(Peers, [S,Sig])),
          S ! {maxTreeFragment, self()},
          Sig ! {minTreeFragment, self()},
          {NewRtree, {MinX, MinY}, {MaxX, MaxY}} = waitForReplies(),
          startNewServer(Name, {MinX, MinY}, {MaxX, MaxY}, NewRtree, MaxRange, Peers, Sig),
          % S ! ok,
          % Sig ! ok,
          sendOk(Peers),
          notifyNewServer(Name, Peers)
        end).


waitForReplies() ->
  receive
    {minRange, NewMinRtreeForPid, MinRangeForPid} ->
      receive
        {maxRange, NewMaxRtreeForPid, MaxRangeForPid} ->
          Tree = i3RTree:mergeTrees(NewMinRtreeForPid,NewMaxRtreeForPid),
          Res = {Tree, MinRangeForPid, MaxRangeForPid};
        Other ->
          io:format("receive error ~w~n", [Other]),
          Res = ok
      end;
    {maxRange, NewMaxRtreeForPid, MaxRangeForPid} ->
      receive
        {minRange, NewMinRtreeForPid, MinRangeForPid} ->
          Tree = i3RTree:mergeTrees(NewMinRtreeForPid,NewMaxRtreeForPid),
          Res = {Tree, MinRangeForPid, MaxRangeForPid};
        Other ->
          io:format("receive error ~w~n", [Other]),
          Res = ok
      end
  end,

  Res.


getWeight(Peers) ->
  lists:map(fun(Peer) -> Peer ! {weight, self()} end, Peers),
  receiveReplies(length(Peers), []).

notifyNewServer(Name, Peers) ->
  lists:map(fun(Peer) -> Peer ! {newServer, Name} end, Peers).

sendOk(Peers) ->
  lists:map(fun(Peer) -> Peer ! ok end, Peers).

receiveReplies(0, Replies) ->
  Replies;

receiveReplies(Peers, Replies) ->
  receive
    {weightResult, Pid, Next, Weight} ->
      Res = Replies ++ [{Pid, Next, Weight}];
    _ ->
      Res = Replies
  end,
  receiveReplies(Peers -1, Res).


maxWeight(Replies) ->
% cada uno tiene - > {Pid, Range, Weight}
F = fun({Pid, Next, Weight}, {PidMax, NextMax, WeightMax}) ->
    if
      Weight >= WeightMax ->
        Res = {Pid, Next, Weight};
      true ->
        Res = {PidMax, NextMax, WeightMax}
    end,
    Res
  end,
 {Pid, Next, Weight} = lists:foldl(F, {0,0,0}, Replies),

 {Pid, Next}.


stop(Server) ->
Server ! stop.

server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY}) ->
  receive

    {newServer, Name} ->
      io:format("New peer: ~w~n", [Peers ++ [Name]]),
      server(MyName, Peers ++ [Name], Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY});

    {weight, Pid} ->
      spawn(fun() ->  weight(I3Rtree, Pid, MyName, Next) end),
      receive
        {maxTreeFragment, Pid} ->
          MinRangeForPid = {InitialX/2, FinalY/2},
          {NewRtreeForPid, MyNewRtree} = i3RTree:partitionalTree(MinRangeForPid, {MaxRangeX,MaxRangeY}, I3Rtree),
          Pid ! {minRange, NewRtreeForPid, MinRangeForPid},
          Nnext = Pid,% Ojo aca si quiero probar !
          receive
            ok ->
              ok
          end;

        {minTreeFragment, Pid} ->
          MaxRangeForPid = {FinalX/2, FinalY/2}, % no estoy seguro.
          {NewRtreeForPid, MyNewRtree} = i3RTree:partitionalTree({InitialX, InitialY}, MaxRangeForPid, I3Rtree),
          Pid ! {maxRange, NewRtreeForPid, MaxRangeForPid},
          Nnext = Next,
          receive
            ok ->
              ok
          end;

        ok ->
          MyNewRtree = I3Rtree,
          Nnext = Next
      end,
      server(MyName, Peers, Nnext, MyNewRtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY});

    {subscribe, Pid, {X, Y}} ->
      case rangeBelong({X, Y}, {0,0}, {MaxRangeX, MaxRangeY}) of
        true ->
          case rangeBelong({X, Y}, {InitialX, InitialY}, {FinalX, FinalY}) of
            true ->
              NRtree = i3RTree:subscribe(Pid, {X, Y}, timeNow(), I3Rtree),
              Pid ! ok;
            false ->
              NRtree = I3Rtree,
              Next ! {subscribe, Pid, {X, Y}}
          end;
        false ->
          NRtree = I3Rtree
      end,
      server(MyName, Peers, Next, NRtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY});

    {unsubscribe, Pid} ->
      case pidBelong(Pid, I3Rtree) of
        true ->
          NRtree = i3RTree:unsubscribe(Pid, I3Rtree),
          Pid ! ok;
        false ->
          NRtree = I3Rtree,
          Next ! {unsubscribe, Pid}
      end,
      server(MyName, Peers, Next, NRtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY});

    {move, Pid, {X, Y}} ->
      case rangeBelong({X, Y}, {0,0}, {MaxRangeX, MaxRangeY}) of
        true ->
          case pidBelong(Pid, I3Rtree) of
            true ->
              case rangeBelong({X, Y}, {InitialX, InitialY}, {FinalX, FinalY}) of
                true ->
                  NRtree = i3RTree:move(Pid, {X,Y}, timeNow(), I3Rtree);
                false ->
                  NRtree1 = i3RTree:move(Pid, {X,Y}, timeNow(), I3Rtree),
                  NRtree = i3RTree:unsubscribe(Pid, NRtree1),
                  Next ! {move, Pid, {X, Y}}
              end;
            false ->
              case rangeBelong({X, Y}, {InitialX, InitialY}, {FinalX, FinalY}) of
                true ->
                  NRtree = i3RTree:subscribe(Pid, {X, Y}, timeNow(), I3Rtree);
                false ->
                  NRtree = I3Rtree,
                  Next ! {move, Pid, {X, Y}}
              end
          end;
        false ->
          io:format("out of bound"),
          NRtree = I3Rtree
      end,
      % io:format("tree: ~w~n", [NRtree]),
      server(MyName, Peers, Next, NRtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY});

    {timelapse, Region, Instant, Sender, ReplyTo} ->
      case lists:member(Sender, Peers) of
        true ->
          spawn(fun() ->
                  timelapse_query(Region, Instant, ReplyTo, I3Rtree, [], 0)
                end);
        false ->
          spawn(fun() ->
                  lists:foreach(fun(Peer) -> Peer ! {timelapse, Region, Instant, MyName, self()} end, Peers),
                  timelapse_query(Region, Instant, ReplyTo, I3Rtree, [], length(Peers))
                end)
      end,
      server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY});

    {interval, Region, {Ti,Tk}, Sender, ReplyTo} ->
      case lists:member(Sender, Peers) of
        true ->
          spawn(fun() ->
                  interval_query(Region, {Ti,Tk}, ReplyTo, I3Rtree, [], 0)
                end);
        false ->
          spawn(fun() ->
                  lists:foreach(fun(Peer) -> Peer ! {interval, Region, {Ti,Tk}, MyName, self()} end, Peers),
                  interval_query(Region, {Ti,Tk}, ReplyTo, I3Rtree, [], length(Peers))
                end)
      end,
      server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY});

    {event, RegionMin, RegionMax, Sender, ReplyTo} ->
      case lists:member(Sender, Peers) of
        true ->
          spawn(fun() ->
                  event_query(RegionMin, RegionMax, ReplyTo, I3Rtree, [], 0)
                end);
        false ->
          spawn(fun() ->
                  lists:foreach(fun(Peer) -> Peer ! {event, RegionMin, RegionMax, MyName, self()} end, Peers),
                  event_query(RegionMin, RegionMax, ReplyTo, I3Rtree, [], length(Peers))
                end)
      end,
      server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY});

    {track, Pid, {Ti,Tk}, Sender, ReplyTo} ->
      case lists:member(Sender, Peers) of
        true ->
          spawn(fun() ->
                  track_query(Pid, {Ti,Tk}, ReplyTo, I3Rtree, [], 0)
                end);
        false ->
          spawn(fun() ->
                  lists:foreach(fun(Peer) -> Peer ! {track, Pid, {Ti,Tk}, MyName, self()} end, Peers),
                  track_query(Pid, {Ti,Tk}, ReplyTo, I3Rtree, [], length(Peers))
                end)
      end,
      server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY});

    {position, Pid, Sender, ReplyTo} ->
      case pidBelong(Pid, I3Rtree) of
        true ->
          spawn(fun() ->
                  position_query(Pid, ReplyTo, I3Rtree)
                end);
        false ->
          Next ! {position, Pid, Sender, ReplyTo}
      end,
      server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY});

    stop ->
      ok
  end.


timelapse_query(Region, Instant, ReplyTo, I3Rtree, OtherReply, 0) ->
  Reply = i3RTree:timelapse_query(Region, Instant, I3Rtree),
  io:format("Query timeLapse: ~w ~w~n", [Instant, Reply]),
  ReplyTo ! {reply, Reply ++ OtherReply};

timelapse_query(Region, Instant, ReplyTo, I3Rtree, OtherReply, CountPeers) ->
  receive
    {reply, Reply} ->
      timelapse_query(Region, Instant, ReplyTo, I3Rtree, Reply ++ OtherReply, CountPeers-1);
    _ ->
      ok
  end.

interval_query(Region, {Ti,Tk}, ReplyTo, I3Rtree, OtherReply, 0) ->
  Reply = i3RTree:interval_query(Region, {Ti,Tk}, I3Rtree),
  io:format("Query interval: ~w ~w ~w~n", [Region, {Ti,Tk} ,Reply]),
  ReplyTo ! {reply, Reply ++ OtherReply};

interval_query(Region, {Ti,Tk}, ReplyTo, I3Rtree, OtherReply, CountPeers) ->
  receive
    {reply, Reply} ->
      interval_query(Region, {Ti,Tk}, ReplyTo, I3Rtree, Reply ++ OtherReply, CountPeers-1);
    _ ->
      ok
  end.

event_query(RegionMin, RegionMax, ReplyTo, I3Rtree, OtherReply, 0) ->
  Reply = i3RTree:event_query(RegionMin, RegionMax, I3Rtree),
  io:format("Query event: ~w ~w ~w~n", [RegionMin, RegionMax ,Reply]),
  ReplyTo ! {reply, Reply ++ OtherReply};

event_query(RegionMin, RegionMax, ReplyTo, I3Rtree, OtherReply, CountPeers) ->
  receive
    {reply, Reply} ->
      event_query(RegionMin, RegionMax, ReplyTo, I3Rtree, Reply ++ OtherReply, CountPeers-1);
    _ ->
      ok
  end.

track_query(Pid, {Ti,Tk}, ReplyTo, I3Rtree, OtherReply, 0) ->
  Reply = i3RTree:track_query(Pid, {Ti,Tk}, I3Rtree),
  io:format("Query track: ~w ~w ~w~n", [Pid, {Ti,Tk}, Reply]),
  ReplyTo ! {reply, [Reply] ++ OtherReply};

track_query(Pid, {Ti,Tk}, ReplyTo, I3Rtree, OtherReply, CountPeers) ->
  receive
    {reply, Reply} ->
      track_query(Pid, {Ti,Tk}, ReplyTo, I3Rtree, [Reply] ++ OtherReply, CountPeers-1);
    _ ->
      ok
  end.

position_query(Pid, ReplyTo, I3Rtree) ->
  Reply = i3RTree:position_query(Pid, I3Rtree),
  io:format("Query position: ~w ~w~n", [Pid, Reply]),
  ReplyTo ! {reply, Reply}.

rangeBelong({X, Y}, {InitialX, InitialY}, {FinalX, FinalY}) ->
  (X >= InitialX andalso X < FinalX) andalso (Y >= InitialY andalso Y < FinalY).

pidBelong(Pid, I3Rtree) ->
  i3RTree:pidBelong(Pid,I3Rtree).

weight(I3Rtree, Pid, MyName, Next) ->
  Weight = i3RTree:weight(I3Rtree),
  Pid ! {weightResult, MyName, Next, Weight}.


timeNow() ->
  {H, M, S} = erlang:time(),
  H * 3600 + M * 60 + S.
