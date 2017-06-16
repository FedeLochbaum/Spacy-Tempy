-module(distributedServer).
-export([start/3,stop/1,timeNow/0]).
-import(rstar, [rstar/1]).

start(Name, {InitialX, InitialY}, {FinalX, FinalY}) ->
  register(Name, spawn(fun() -> init({InitialX, InitialY}, {FinalX, FinalY}) end)).


init({InitialX, InitialY}, {FinalX, FinalY}) ->
  receive
    {peers, Peers, Next} ->
      server(Peers, Next, i3RTree:new(), {InitialX, InitialY}, {FinalX, FinalY});
    stop ->
      ok
  end.

stop(Server) ->
Server ! stop.

server(Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}) ->
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
      server(Peers, Next, NRtree, {InitialX, InitialY}, {FinalX, FinalY});

    {unsubscribe, Pid} ->
      case pidBelong(Pid, I3Rtree) of
        true ->
          NRtree = i3RTree:unsubscribe(Pid, I3Rtree),
          Pid ! ok;
        false ->
          NRtree = I3Rtree,
          Next ! {unsubscribe, Pid}
      end,
      server(Peers, Next, NRtree, {InitialX, InitialY}, {FinalX, FinalY});

    {move, Pid, {X, Y}} ->
      case pidBelong(Pid, I3Rtree) of
        true ->
          case rangeBelong({X, Y}, {InitialX, InitialY}, {FinalX, FinalY}) of
            true ->
              NRtree = i3RTree:move(Pid, {X,Y}, timeNow(), I3Rtree);
            false ->
              % NRtree = i3RTree:move(Pid, {X,Y}, timeNow(), I3Rtree),
              NRtree = i3RTree:unsubscribe(Pid, I3Rtree),
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
      server(Peers, Next, NRtree, {InitialX, InitialY}, {FinalX, FinalY});

    {timelapse, Region, Instant, Process} ->
      case lists:member(Process, Peers) of
        true ->
          spawn(fun() ->
                  timelapse_query(Region, Instant, Process, I3Rtree, [], 0)
                end);
        false ->
          spawn(fun() ->
                  lists:foreach(fun(Server) -> Server ! {timelapse, Region, Instant, self()} end, Peers),
                  timelapse_query(Region, Instant, Process, I3Rtree,[] ,length(Peers))
                end)
      end,
      server(Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY});

    {interval, Region, {Ti,Tk}, Process} ->
      spawn(fun() ->
              Next ! {interval, Region, {Ti,Tk}, self()},
              interval_query(Region, {Ti,Tk}, Process, I3Rtree, Peers)
            end),
      server(Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY});

    {event, RegionMin, RegionMax, Process} ->
      spawn(fun() ->
              Next ! {event, RegionMin, RegionMax, self()},
              event_query(RegionMin, RegionMax, Process, I3Rtree, Peers)
            end),
      server(Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY});

    {track, Pid, {Ti,Tk}, Process} ->
      spawn(fun() ->
              Next ! {track, Pid, {Ti,Tk}, self()},
              track_query(Pid, {Ti,Tk}, Process, I3Rtree, Peers)
            end),
      server(Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY});

    {position, Pid, Process} ->
      spawn(fun() ->
              Next ! {position, Pid, self()},
              position_query(Pid, Process, I3Rtree, Peers)
            end),
      server(Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY});
    stop ->
      ok
  end.


% HACER LAS DEMAS QUERYES ASI. HAY QUE TESTEAR ESTA PRIMERO ////// SE HACE RECURSIVAMENTE.. 
timelapse_query(Region, Instant, Process, I3Rtree, OtherReply, 0) ->
  Reply = i3RTree:timelapse_query(Region, Instant, I3Rtree),
  io:format("Query timeLapse: ~w ~w~n", [Instant, Reply]),
  Process ! {reply, Reply ++ OtherReply};

timelapse_query(Region, Instant, Process, I3Rtree, OtherReply, CountPeers) ->
  receive
    {reply, Reply} ->
      timelapse_query(Region, Instant, Process, I3Rtree, Reply ++ OtherReply, CountPeers-1);
    _ ->
      ok
  end.

interval_query(Region, {Ti,Tk}, Process, I3Rtree, Peers) ->
  Reply = i3RTree:interval_query(Region, {Ti,Tk}, I3Rtree),
  io:format("Query interval: ~w ~w ~w~n", [Region, {Ti,Tk} ,Reply]),
  Process ! {reply, Reply}.

event_query(RegionMin, RegionMax, Process, I3Rtree, Peers) ->
  Reply = i3RTree:event_query(RegionMin, RegionMax, I3Rtree),
  io:format("Query event: ~w ~w ~w~n", [RegionMin, RegionMax ,Reply]),
  Process ! {reply, Reply}.

track_query(Pid, {Ti,Tk}, Process, I3Rtree, Peers) ->
  Reply = i3RTree:track_query(Pid, {Ti,Tk}, I3Rtree),
  io:format("Query track: ~w ~w ~w~n", [Pid, {Ti,Tk}, Reply]),
  Process ! {reply, Reply}.

position_query(Pid, Process, I3Rtree, Peers) ->
  Reply = i3RTree:position_query(Pid, I3Rtree),
  io:format("Query position: ~w ~w~n", [Pid, Reply]),
  Process ! {reply, Reply}.


rangeBelong({X, Y}, {InitialX, InitialY}, {FinalX, FinalY}) ->
  (X >= InitialX andalso X < FinalX) andalso (Y >= InitialY andalso Y < FinalY).

pidBelong(Pid, I3Rtree) ->
  i3RTree:pidBelong(Pid,I3Rtree).

timeNow() ->
  {H, M, S} = erlang:time(),
  H * 3600 + M * 60 + S.
