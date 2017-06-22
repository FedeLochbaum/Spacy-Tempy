-module(distributedServer).
-export([start/4,stop/1,timeNow/0]).
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

stop(Server) ->
Server ! stop.

server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY}) ->
  receive
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
          io:format("out of bound")
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

timeNow() ->
  {H, M, S} = erlang:time(),
  H * 3600 + M * 60 + S.
