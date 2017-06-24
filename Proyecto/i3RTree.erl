- module(i3RTree).
- export([new/0, subscribe/4, unsubscribe/2, move/4, timelapse_query/3, interval_query/3, event_query/3, track_query/3, region_query/4, position_query/2,pidBelong/2, weight/1]).

new() ->
  Map = maps:new(),
  NMap = maps:put(0,{0,0,{0,0,0},0,0},Map),
  {rstar:new(3),NMap,{0,{0,0,{0,0,0},0,0}}}.

subscribe(Pid, {X, Y}, Instant, {Rtree, Map, {Last,LastTuple}}) ->
  Mbr = {X, Y, Instant},
  P3d = {0,0,0},
  Pa = LastTuple,
  Tuple = {Mbr, Instant, P3d, Pa, 0},
  NMap = updateLast({Pid,Tuple},{Last,LastTuple},Map),
  {Rtree, maps:put(Pid,Tuple,NMap),{Pid,Tuple}}.

unsubscribe(Pid, {Rtree, Map, LastElem} ) ->
  case LastElem of
    {Last,{Mbr, Instant, P3d, Pa, Ps}} ->
      case Pid  == Last of
        true ->
          {MbrOld, InstantOld, {PidOld, MbrOldOld, P3dOld}, PaOld, PsOld} = Pa,
          NewLast = PidOld,
          NewLastTuple = Pa;
        false ->
          NewLast = Last,
          NewLastTuple = {Mbr, Instant, P3d, Pa, Ps}
      end,
      NMap = maps:remove(Last,Map),
      {Rtree, NMap, {NewLast,NewLastTuple}};
    _ ->
      {Rtree, Map, LastElem}
  end.

weight({Rtree, Map, {Last,LastTuple}}) ->
  maps:size(Map).

move(Pid, {X,Y}, Instant, {Rtree, Map, {Last,LastTuple}}) ->
  Mbr = {X, Y, Instant},
  Pa = LastTuple,
  {{XOld, YOld, InstantOld}, InstantOld, P3dOld, PaOld, PsOld} = maps:get(Pid,Map),

  Time = (Instant - InstantOld),
  P3d = {Pid, {XOld, YOld, Time}, P3dOld},
  Tuple = {Mbr, Instant, P3d, Pa, 0},

  NMap = updateLast({Pid,Tuple},{Last,LastTuple},Map),

  Point = rstar_geometry:point3d(XOld, YOld, InstantOld, P3dOld),
  NewRtree = rstar:insert(Rtree, Point),
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

position_query(Pid, {_, Map, _}) ->
    case maps:get(Pid, Map, none) of
      none ->
        Res = none;
      Reply ->
        Res = parseAReply(Reply)
    end,
    Res.

track_query(Pid, {Ti,Tk}, {Rtree, Map, {Last,LastTuple}}) ->
  case pidBelong(Pid, {Rtree, Map, {Last,LastTuple}}) of
    true ->
      {Mbr, Time, {Name,WaitTime,P3d}, Pa, Ps} = maps:get(Pid,Map),
      {Name, getPath({Mbr, Time, {Name,WaitTime,P3d}, Pa, Ps}, {Ti,Tk})};
    false ->
      {}
  end.

pidBelong(Pid, {_, Map, _}) ->
  case maps:get(Pid, Map, false) of
    false ->
      false;
    _ ->
      true
  end.

getPath({Mbr, Time, P3d, Pa, Ps}, {Ti,Tk}) ->
  case Time >= Ti andalso Time =< Tk of
    true ->
      getPath(Pa, {Ti,Tk}) ++ [Mbr];
    false ->
      getPath(Pa, {Ti,Tk})
  end;

getPath(_, {Ti,Tk}) ->
  [].

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

isPartOfRegion({X0, Y0, T}, {Xi, Yi}, {Xf,Yf}) ->
  isPart({X0, Y0, T}, Xf, Yf) andalso X0 >= Xi andalso Y0 >= Yi.

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

parseAReply(Reply) ->
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
  Res.


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


% lastForNewPartitionalTree(Map, LastElem, {InitialX, InitialY}, {FinalX, FinalY}) ->
%   case LastElem of
%     {Last,{{X,Y,T}, Instant, P3d, Pa, Ps}} ->
%       case isPartOfRegion({X,Y,T}, {InitialX, InitialY}, {FinalX, FinalY})  of
%         true ->
%           Res = {LastElem, Ps};
%         false ->
%           Res = {0,0},
%           lastForNewPartitionalTree(Map, Pa, {InitialX, InitialY}, {FinalX, FinalY})
%       end;
%     _ ->
%       Res = {0, 0}
%   end,
%   Res.
%
% partitionalMap({InitialX, InitialY}, {FinalX, FinalY}, Map) ->
%   F = fun(K,V) ->
%       case V of
%         {{X,Y,T}, Instant, P3d, Pa, Ps} ->
%           Res = isPartOfRegion({X,Y,T}, {InitialX, InitialY}, {FinalX, FinalY});
%         _ ->
%           Res = false
%       end,
%       Res
%       end,
%    Nmap = maps:filter(F,Map),
%    Names =  maps:keys(Nmap),
%    {Nmap, removeKeys(Names,Map)}.
%
%
%  removeKeys(Names, Map) ->
%    F = fun(Name, AccMap) ->
%         maps:remove(Name,AccMap)
%       end,
%    lists:foldl(F, Map, Names).

% partitionalTree({InitialX, InitialY}, {FinalX, FinalY}, {Rtree, Map, LastElem}) ->
  % cuando temrine esto, descomentar el next en el server.
  % {NewLastTuple, OldLastTuple} = lastForNewPartitionalTree(Map, LastElem, {InitialX, InitialY}, {FinalX, FinalY}),
  % {Nmap, NewOldMap} = partitionalMap({InitialX, InitialY}, {FinalX, FinalY}, Map),

  % {{rstar:new(3), Nmap, NewLastTuple}, {Rtree, NewOldMap, OldLastTuple}}.
  % {i3RTree:new(),{Rtree, Map, LastElem}}.
%Me parece mejor crear un nuevo arbol pero solo transpasarle los otros datos.

% mergeTrees(R1,R2) -> % faltan implementar
  % R1.
