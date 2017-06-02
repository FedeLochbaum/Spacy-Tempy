- module(i3RTree).
- export([new/0, subscribe/4, unsubscribe/2, move/4, timelapse_query/3, interval_query/3, event_query/2, track_query/2]).


new() ->
  Map = maps:new(),
  NMap = maps:put(0,{0,0,0,0,0},Map),
  {rstar:new(3),NMap,{0,{0,0,0,0,0}}}.

subscribe(Pid, {X, Y}, Instant, {Rtree, Map, {Last,LastTuple}}) ->
  Mbr = {X, Y, Instant},
  P3d = {0,0,0},
  Pa = LastTuple,
  Tuple = {Mbr, Instant, P3d, Pa, 0},
  NMap = updateLast({Pid,Tuple},{Last,LastTuple},Map),
  {Rtree, maps:put(Pid,Tuple,NMap),{Pid,Tuple}}.

unsubscribe(Pid, {Rtree, Map, {Last,LastTuple}} ) ->
  % NOT IMPLEMENTED
  {Rtree, Map, {Last,LastTuple}}.

move(Pid, {X,Y}, Instant, {Rtree, Map, {Last,LastTuple}}) ->

  Mbr = {X, Y, Instant},
  Pa = LastTuple,
  {{XOld, YOld, InstantOld}, InstantOld, P3dOld, PaOld, PsOld} = maps:get(Pid,Map),

  Time = (Instant - InstantOld),
  P3d = {Pid, {XOld, YOld, Time}, P3dOld},
  Tuple = {Mbr, Instant, P3d, Pa, 0},

  NMap = updateLast({Pid,Tuple},{Last,LastTuple},Map),

  Point = rstar_geometry:point3d(XOld, YOld, Time, P3dOld),
  NewRtree = rstar:insert(Rtree, Point),

  {NewRtree, NMap, {Pid,Tuple}}.

timelapse_query({X,Y}, Instant, {Rtree, Map, {Last,LastTuple}}) ->
  Point = rstar_geometry:point3d(X, Y, Instant, ok),
  rstar:search_around(Rtree, Point, 0.0).

interval_query({X,Y}, {Ti,Tk}, {Rtree, Map, {Last,LastTuple}}) ->
  Box = rstar_geometry:new(3, [{X, X}, {Y, Y}, {Ti,Tk}], ok),
  rstar:search_within(Rtree, Box).

event_query(Region, {Rtree, Map, {Last,LastTuple}}) ->
  % NOT IMPLEMENTED
  Rtree.

track_query(Pid, {Rtree, Map, {Last,LastTuple}}) ->
  % NOT IMPLEMENTED
  Rtree.

updateLast({Pid,Tuple},{Last,{Mbr,T,P3d,Pa,_}},Map) ->
  maps:put(Last,{Mbr,T,P3d,Pa,Tuple},Map).
