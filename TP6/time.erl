-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1,update/3,safe/2]).

% zero(): retorna un valor Lamport inicial (puede ser 0).
zero() ->
  0.
% inc(Name, T): retorna el tiempo T incrementado en uno (probablemente ignoremos el Name, pero lo usaremos más adelante).
inc(_, T) ->
  T+1.
% merge(Ti, Tj): unifica los dos timestamps Lamport (eso es, toma el mayor).
merge(Ti, T) ->
  if
    Ti > T -> Ti;
    true  ->  T
  end.

% leq(Ti, Tj): retorna true si Ti es menor o igual a Tj.
leq(Ti, Tj) ->
  Ti =< Tj.


%: retorna un reloj que pueda llevar cuenta de los nodos
clock(Nodes) ->
  M = maps:from_list([]),
  insertNodes(Nodes,M).

insertNodes([],Map) ->
  Map;
insertNodes([Name|Nodes],Map) ->
  insertNodes(Nodes,maps:put(Name,zero(),Map)).

% : retorna un reloj que haya sido actualizado
update(Node, Time, Clock) ->
  NodeTime = maps:get(Node,Clock),
  case Time > NodeTime of
    true ->
      maps:put(Node,Time,Clock);
    false ->
      Clock
  end.


% : retorna true o false si es seguro enviar el mensaje de log de un evento que ocurrió en el tiempo Time dado.
safe(Time, Clock) ->
  safeTimeOfList(maps:to_list(Clock),Time).


safeTimeOfList([],_) ->
  true;

safeTimeOfList([{_,TimeName}|Tail],Time) ->
  (Time < TimeName) andalso (safeTimeOfList(Tail,Time)).
