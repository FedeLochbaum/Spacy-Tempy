-module(logger).
-export([start/1, stop/1]).


start(Nodes) ->
  spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  loop(time:clock(Nodes),queue:new()).

loop(Clock,Queue) ->
  receive
    {log, From, Time, Msg} ->
      UpdateClock = time:update(From,Time,Clock),
      UpdateQueue = showMsgs(queue:snoc(Queue,{From,Time,Msg}),UpdateClock),
      loop(UpdateClock,UpdateQueue);
    stop ->
      ok
  end.


showMsgs(Queue,Clock) ->
  case(queue:is_empty(Queue)) of
    true ->
      Queue;
    false ->
      {From,Time,Msg} = queue:head(Queue),
      Dequeue = queue:tail(Queue),
      case (time:safe(Time,Clock)) of
        true ->
          log(From,Time,Msg),
          showMsgs(Dequeue,Clock);
        false ->
          queue:snoc(showMsgs(Dequeue,Clock),{From,Time,Msg})
      end
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
