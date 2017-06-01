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
  % inicio actualizar el map,
  {{XOld, YOld, InstantOld}, InstantOld, P3dOld, PaOld, PsOld} = maps:get(Pid,Map),

  Mbr = {X, Y, Instant},
  P3d = {Pid, {XOld, YOld, InstantOld}, P3dOld},
  Pa = LastTuple,
  Tuple = {Mbr, Instant, P3d, Pa, 0},
  NMap = updateLast({Pid,Tuple},{Last,LastTuple},Map),
  % fin actualizar map
  % inicio actualizar Rtree
  Point = rstar_geometry:point3d(XOld, YOld, InstantOld, P3d),
  NewRtree = rstar:insert(Rtree, Point),
  % fin actualizar Rtree
  {NewRtree, NMap, {Pid,Tuple}}.

timelapse_query(Region, Instant, {Rtree, Map, {Last,LastTuple}}) ->
  % NOT IMPLEMENTED
  [].

interval_query(Region, {Ti,Tk}, {Rtree, Map, {Last,LastTuple}}) ->
  % NOT IMPLEMENTED
  [].

event_query(Region, {Rtree, Map, {Last,LastTuple}}) ->
  % NOT IMPLEMENTED
  [].

track_query(Pid, {Rtree, Map, {Last,LastTuple}}) ->
  % NOT IMPLEMENTED
  [].

updateLast({Pid,Tuple},{Last,{Mbr,T,P3d,Pa,_}},Map) ->
  maps:put(Last,{Mbr,T,P3d,Pa,Tuple},Map).
