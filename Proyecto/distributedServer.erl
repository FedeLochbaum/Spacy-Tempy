-module(distributedServer).
-export([start/3,stop/1,timeNow/0]).
-import(rstar, [rstar/1]).

start(Name, {InitialX, InitialY}, {FinalX, FinalY}) ->
  register(Name, spawn(fun() -> init(Name, {InitialX, InitialY}, {FinalX, FinalY}) end)).


init(Name, {InitialX, InitialY}, {FinalX, FinalY}) ->
  receive
    {peers, Peers, Next} ->
      server(Name, Peers, Next, i3RTree:new(), {InitialX, InitialY}, {FinalX, FinalY});
    stop ->
      ok
  end.

stop(Server) ->
Server ! stop.

server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}) ->
  receive
    {subscribe, Pid, {X, Y}} ->
      case rangeBelong({X, Y}, {InitialX, InitialY}, {FinalX, FinalY}) of
        true ->
          NRtree = i3RTree:subscribe(Pid, {X, Y}, timeNow(), I3Rtree),
          Pid ! ok;
        false ->
          NRtree = I3Rtree,
          Next ! {subscribe, Pid, {X, Y}}
      end,
      server(MyName, Peers, Next, NRtree, {InitialX, InitialY}, {FinalX, FinalY});

    {unsubscribe, Pid} ->
      case pidBelong(Pid, I3Rtree) of
        true ->
          NRtree = i3RTree:unsubscribe(Pid, I3Rtree),
          Pid ! ok;
        false ->
          NRtree = I3Rtree,
          Next ! {unsubscribe, Pid}
      end,
      server(MyName, Peers, Next, NRtree, {InitialX, InitialY}, {FinalX, FinalY});

    {move, Pid, {X, Y}} ->
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
      end,
      io:format("tree: ~w~n", [NRtree]),
      server(MyName, Peers, Next, NRtree, {InitialX, InitialY}, {FinalX, FinalY});

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
      server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY});

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
      server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY});

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
      server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY});

    {track, Pid, {Ti,Tk}, Process} ->
      spawn(fun() ->
              Next ! {track, Pid, {Ti,Tk}, self()},
              track_query(Pid, {Ti,Tk}, Process, I3Rtree, Peers)
            end),
      server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY});

    {position, Pid, Process} ->
      spawn(fun() ->
              Next ! {position, Pid, self()},
              position_query(Pid, Process, I3Rtree, Peers)
            end),
      server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY});
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

track_query(Pid, {Ti,Tk}, Process, I3Rtree, Peers) ->
  Reply = i3RTree:track_query(Pid, {Ti,Tk}, I3Rtree),
  io:format("Query track: ~w ~w ~w~n", [Pid, {Ti,Tk}, Reply]),
  Process ! {reply, Reply}.

position_query(Pid, Process, I3Rtree, Peers) ->
  Reply = i3RTree:position_query(Pid, I3Rtree),
  io:format("Query position: ~w ~w~n", [Pid, Reply]),
  Process ! {reply, Reply}.


rangeBelong({X, Y}, {InitialX, InitialY}, {FinalX, FinalY}) -> % Parece haber un bug, revisar geometricamente
  (X >= InitialX andalso X < FinalX) andalso (Y >= InitialY andalso Y < FinalY).

pidBelong(Pid, I3Rtree) ->
  i3RTree:pidBelong(Pid,I3Rtree).

timeNow() ->
  {H, M, S} = erlang:time(),
  H * 3600 + M * 60 + S.