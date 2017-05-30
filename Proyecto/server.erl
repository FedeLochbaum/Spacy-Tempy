-module(server).
-export([start/1,stop/1]).
-import(rstar, [rstar/1]).

start(Name) ->
  register(Name, spawn(fun() -> server(i3RTree:new()) end)).

stop(Server) ->
Server ! stop.

server(I3Rtree) ->
  receive
    {subscribe, Pid, {X, Y}, Instant} ->
      NRtree = i3RTree:subscribe(Pid, {X, Y}, Instant, I3Rtree),
      Pid ! ok,
      server(NRtree);
    {unsubscribe, Pid} ->
      NRtree = i3RTree:unsubscribe(Pid, I3Rtree),
      Pid ! ok,
      server(NRtree);
    {move, Pid, {X, Y}, Instant} ->
      NRtree = i3RTree:move(Pid, {X,Y}, Instant, I3Rtree),
      server(NRtree);
    {timelapse, Region, Instant, Process} -> % distribuido
      spawn(fun() -> timelapse_query(Region, Instant, Process, I3Rtree) end),
      server(I3Rtree);
    {interval, Region, {Ti,Tk}, Process} -> % distribuido
      spawn(fun() -> interval_query(Region, {Ti,Tk}, Process, I3Rtree) end),
      server(I3Rtree);
    {event, Region, Process} -> % distribuido
      spawn(fun() -> event_query(Region, Process, I3Rtree) end),
      server(I3Rtree);
    {track, Pid, Process} -> % distribuido
      spawn(fun() -> track_query(Pid, Process, I3Rtree) end),
      server(I3Rtree);
    stop ->
      ok
  end.


timelapse_query(Region, Instant, Process, I3Rtree) ->
  Reply = i3RTree:timelapse_query(Region, Instant, I3Rtree),
  Process ! {reply, Reply}.

interval_query(Region, {Ti,Tk}, Process, I3Rtree) ->
  Reply = i3RTree:interval_query(Region, {Ti,Tk}, I3Rtree),
  Process ! {reply, Reply}.

event_query(Region, Process, I3Rtree) ->
  Reply = i3RTree:event_query(Region, I3Rtree),
  Process ! {reply, Reply}.

track_query(Pid, Process, I3Rtree) ->
  Reply = i3RTree:track_query(Pid, I3Rtree),
  Process ! {reply, Reply}.
