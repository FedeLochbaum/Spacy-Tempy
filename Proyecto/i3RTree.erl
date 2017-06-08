- module(i3RTree).
- export([new/0, subscribe/4, unsubscribe/2, move/4, timelapse_query/3, interval_query/3, event_query/3, track_query/2, region_query/4]).

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
  io:format("Move : ~w~n", [Instant]),
  Mbr = {X, Y, Instant},
  Pa = LastTuple,
  {{XOld, YOld, InstantOld}, InstantOld, P3dOld, PaOld, PsOld} = maps:get(Pid,Map),

  Time = (Instant - InstantOld),
  P3d = {Pid, {XOld, YOld, Time}, P3dOld},
  Tuple = {Mbr, Instant, P3d, Pa, 0},

  NMap = updateLast({Pid,Tuple},{Last,LastTuple},Map),

  Point = rstar_geometry:point3d(XOld, YOld, InstantOld, P3dOld),
  NewRtree = rstar:insert(Rtree, Point),
  io:format("Finaly Move : ~w~n", [NewRtree]),
  {NewRtree, maps:put(Pid,Tuple,NMap), {Pid,Tuple}}.

timelapse_query({X,Y}, Instant, {Rtree, Map, {Last,LastTuple}}) ->
  interval_query({X,Y}, {Instant,Instant}, {Rtree, Map, {Last,LastTuple}}).

interval_query({X,Y}, {Ti,Tk}, {Rtree, Map, {Last,LastTuple}}) ->
  region_query({X,Y},{X,Y}, {Ti,Tk}, {Rtree, Map, {Last,LastTuple}}).

event_query({Xmin, Ymin}, {Xmax, Ymax}, {Rtree, Map, {Last,{Mbr,Tmax, P3d, Pa, Ps}}}) ->
  region_query({Xmin,Ymin},{Xmax,Ymax}, {0,Tmax}, {Rtree, Map, {Last,{Mbr,Tmax, P3d, Pa, Ps}}}).

region_query({Xmin,Ymin}, {Xmax, Ymax}, {Ti,Tk}, {Rtree, Map, {Last,LastTuple}}) ->
  case Ti > tMax3D(Rtree) of
    true ->
      Res = lookUpInMap(Xmin,Ymin,Ti,Map);
    false ->
      Region = rstar_geometry:new(3, [{Xmin, Xmax}, {Ymin, Ymax}, {Ti, Tk}], ok),
      case Ti < tMinInd(Map) of
        true ->
          Res = rstar:search_within(Rtree,Region);
        false ->
          ResLookUp = lookUpInMap(Xmin,Ymin,Ti,Map),
          ResSearch = rstar:search_within(Rtree,Region),
          Res = ResLookUp ++ ResSearch
      end
  end,
  parseReply(Res).





track_query(Pid, {Rtree, Map, {Last,LastTuple}}) ->
  % NOT IMPLEMENTED
  Rtree.

lookUpInMap(X, Y, Instant, Map) ->
  [FirstKey | Tail] = maps:keys(Map),
  First = maps:get(FirstKey,Map),
  Fun = fun(K,{Mbr, Time, P3d, Pa, Ps}, Acc) ->
          if
            Time =< Instant ->
              case isPart(Mbr, X, Y) of
                true -> Res = lists:append([Acc,[{Mbr, Time, P3d, Pa, Ps}]]);
                false -> Res = Acc
              end;
            true -> Res = Acc
          end,
          Res
        end,

  maps:fold(Fun, [], Map).

isPart(0, X, Y) ->
  false;

isPart({X0, Y0, _}, X, Y) ->
  X0 =< X andalso Y0 =< Y.

tMax3D({_,_,_,{geometry,3,[_,_,{_,TMax}],_}}) ->
  TMax.

tMinInd(Map) ->
  Fun = fun(K,{Mbr, Time, P3d, Pa, Ps}, Acc) ->
          case Acc > Time of
            true -> Res = Time;
            false -> Res = Acc
          end,
          Res
        end,
  maps:fold(Fun, inf, Map).

updateLast({Pid,Tuple},{Last,{Mbr,T,P3d,Pa,_}},Map) ->
  maps:put(Last,{Mbr,T,P3d,Pa,Tuple},Map).


parseReply(Reply) ->
  Fun = fun(Reply) ->
          case Reply of
            {geometry,3,[{Xmin, _},{Ymin, _},{Tmin,_}],Value} ->
              case Value of
                {Name,{Xold,Yold,IntervalWaitTime}, P3d} ->
                  Res = {{name, Name},{position, Xmin, Ymin, Tmin},{waitTime, IntervalWaitTime}};
                {0,0,0} ->
                  Res = {{name, 0},{position, Xmin, Ymin, Tmin},{waitTime, 0}}
              end;

            {{X, Y, T}, Instant, {Name,_,_}, Pa, Ps} ->
              Res = {{name, Name},{position, X, Y, T},{waitTime, 0}}
          end,
          Res
        end,
  lists:map(Fun,Reply).
