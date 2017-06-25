-module(distributedServer).
-export([start/4,stop/1,timeNow/0, addServer/3, addServerv2/3]).
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

addServerv2(Name, Peers, MaxRange) ->
  register(Name, spawn(
        fun() ->
          Replies = getWeight(Peers),
          {S,Sig} = maxWeight(Replies),
          io:format("Selected Servers are ~w~n", [S]),
          sendOk(lists:subtract(Peers, [S])),
          S ! {myPrevious, self(), Name},
          {NewRtree, {MinX, MinY}, {MaxX, MaxY}} = waitForRepliesv2(),
          notifyNewServer(Name, Peers),
          io:format(" Start New Serve : ~w ~w ~w ~w ~n", [Name, {MinX, MinY}, {MaxX, MaxY}, Sig]),
          server(Name, Peers, Sig, NewRtree,  {MinX, MinY}, {MaxX, MaxY}, MaxRange)
        end)).

addServer(Name, Peers, MaxRange) ->
  register(Name, spawn(
        fun() ->
          Replies = getWeight(Peers),
          {S,Sig} = maxWeight(Replies),
          io:format("Selected Servers are ~w~n", [{S,Sig}]),
          sendOk(lists:subtract(Peers, [S,Sig])),
          S ! {myPrevious, self(), Name},
          Sig ! {myNext, self()},
          {NewRtree, {MinX, MinY}, {MaxX, MaxY}} = waitForReplies(),
          notifyNewServer(Name, Peers),
          io:format(" Start New Serve : ~w ~w ~w ~w ~n", [Name, {MinX, MinY}, {MaxX, MaxY}, Sig]),
          server(Name, Peers, Sig, NewRtree,  {MinX, MinY}, {MaxX, MaxY}, MaxRange)
        end)).



waitForRepliesv2() ->
  receive
    {myPrevious, {MyPreviousInitialX, MyPreviousInitialY}, {MyPreviousFinalX, MyPreviousFinalY}, MyPrevious} ->
        Res = calculateNewRangesv2({ {MyPreviousInitialX, MyPreviousInitialY}, {MyPreviousFinalX, MyPreviousFinalY}, MyPrevious });
    Other ->
      io:format("receive error ~w~n", [Other]),
      Res = ok
  end,
  Res.

waitForReplies() ->
  receive
    {myPrevious, {MyPreviousInitialX, MyPreviousInitialY}, {MyPreviousFinalX, MyPreviousFinalY}, MyPrevious} ->
      receive
        {myNext, {MyNextInitialX, MyNextInitialY}, {MyNextFinalX, MyNextFinalY}, MyNext} ->

          Res = calculateNewRanges({ {MyPreviousInitialX, MyPreviousInitialY}, {MyPreviousFinalX, MyPreviousFinalY}, MyPrevious},
                                      { {MyNextInitialX, MyNextInitialY}, {MyNextFinalX, MyNextFinalY}, MyNext});

        Other ->
          io:format("receive error ~w~n", [Other]),
          Res = ok
      end;
    {myNext, {MyNextInitialX, MyNextInitialY}, {MyNextFinalX, MyNextFinalY}, MyNext} ->
      receive
        {myPrevious, {MyPreviousInitialX, MyPreviousInitialY}, {MyPreviousFinalX, MyPreviousFinalY}, MyPrevious} ->

          Res = calculateNewRanges({ {MyPreviousInitialX, MyPreviousInitialY}, {MyPreviousFinalX, MyPreviousFinalY}, MyPrevious},
                                      { {MyNextInitialX, MyNextInitialY}, {MyNextFinalX, MyNextFinalY}, MyNext});

        Other ->
          io:format("receive error ~w~n", [Other]),
          Res = ok
      end;
    Other ->
      io:format("receive error ~w~n", [Other]),
      Res = ok
  end,
  Res.


calculateNewRangesv2({{MyPreviousInitialX, MyPreviousInitialY}, {MyPreviousFinalX, MyPreviousFinalY}, MyPrevious}) ->
  MyInitialX = MyPreviousInitialX,
  MyInitialY = MyPreviousInitialY,
  MyFinalX   = (MyPreviousInitialX + MyPreviousFinalX) /2,
  MyFinalY   = MyPreviousFinalY,

  io:format("nueva configuracion de server ~w~n", [{ok, {MyFinalX, MyPreviousInitialY}, {MyPreviousFinalX, MyPreviousFinalY} }]),
  MyPrevious ! {ok, {MyFinalX, MyPreviousInitialY}, {MyPreviousFinalX, MyPreviousFinalY} },

  {i3RTree:new(), {MyInitialX, MyInitialY}, {MyFinalX, MyFinalY}}.

calculateNewRanges({ {MyPreviousInitialX, MyPreviousInitialY}, {MyPreviousFinalX, MyPreviousFinalY}, MyPrevious}, { {MyNextInitialX, MyNextInitialY}, {MyNextFinalX, MyNextFinalY}, MyNext}) ->
  io:format("Los servers son ~w y  ~w~n", [{ {MyPreviousInitialX, MyPreviousInitialY}, {MyPreviousFinalX, MyPreviousFinalY}, MyPrevious}, { {MyNextInitialX, MyNextInitialY}, {MyNextFinalX, MyNextFinalY}, MyNext}]),
  if
    MyPreviousInitialX =< MyNextInitialX andalso MyPreviousInitialY =< MyNextInitialY ->

      MyInitialX = (MyNextInitialX + MyPreviousInitialX) / 2,
      MyInitialY = (MyNextInitialY + MyPreviousInitialY) / 2,
      MyFinalX   = (MyNextFinalX   + MyPreviousFinalX)   / 2,
      MyFinalY   = (MyNextFinalY   + MyPreviousFinalY)   / 2,

      % io:format("myPrevious ~w~n", [{ok, {MyPreviousInitialX, MyPreviousInitialY}, {MyPreviousFinalX, MyInitialY} }]),
      MyPrevious ! {ok, {MyPreviousInitialX, MyPreviousInitialY}, {MyPreviousFinalX, MyInitialY} },

      % io:format("myNext ~w~n", [{ok, {MyNextInitialX, MyFinalY}, {MyNextFinalX, MyNextFinalY} }]),
      MyNext ! {ok, {MyNextInitialX, MyFinalY}, {MyNextFinalX, MyNextFinalY} };

    true ->

      MyInitialX = (MyPreviousInitialX + MyNextInitialX) / 2,
      MyInitialY = (MyPreviousInitialY + MyNextInitialY) / 2,
      MyFinalX   = (MyPreviousFinalX   + MyNextFinalX)   / 2,
      MyFinalY   = (MyPreviousFinalY   + MyNextFinalY)   / 2,

      % io:format("myPrevious ~w~n", [{ok, {MyFinalX, MyPreviousInitialY}, {MyPreviousFinalX, MyPreviousFinalY}} ]),
      MyPrevious ! {ok, {MyFinalX, MyPreviousInitialY}, {MyPreviousFinalX, MyPreviousFinalY} },

      % io:format("myNext ~w~n", [{ok, {MyNextInitialX, MyNextInitialY}, {MyInitialX, MyNextFinalY}} ]),
      MyNext ! {ok, {MyNextInitialX, MyNextInitialY}, {MyInitialX, MyNextFinalY} }
  end,
  {i3RTree:new(), {MyInitialX, MyInitialY}, {MyFinalX, MyFinalY}}.



getWeight(Peers) ->
  lists:map(fun(Peer) -> Peer ! {weight, self()} end, Peers),
  receiveReplies(length(Peers), []).

notifyNewServer(Name, Peers) ->
  lists:map(fun(Peer) -> Peer ! {newServer, Name} end, Peers).

sendOk(Peers) ->
  lists:map(fun(Peer) -> Peer ! ok end, Peers).

receiveReplies(0, Replies) ->
  Replies;

receiveReplies(Peers, Replies) ->
  receive
    {weightResult, Pid, Next, Weight} ->
      Res = Replies ++ [{Pid, Next, Weight}];
    _ ->
      Res = Replies
  end,
  receiveReplies(Peers -1, Res).


maxWeight(Replies) ->
% cada uno tiene - > {Pid, Range, Weight}
F = fun({Pid, Next, Weight}, {PidMax, NextMax, WeightMax}) ->
    if
      Weight >= WeightMax ->
        Res = {Pid, Next, Weight};
      true ->
        Res = {PidMax, NextMax, WeightMax}
    end,
    Res
  end,
 {Pid, Next, Weight} = lists:foldl(F, {0,0,0}, Replies),

 {Pid, Next}.


stop(Server) ->
Server ! stop.

server(MyName, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY}) ->
  receive

    {newServer, Name} ->
      io:format("~w New peer: ~w~n", [MyName, Peers ++ [Name]]),
      server(MyName, Peers ++ [Name], Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY});

    {weight, Pid} ->
      spawn(fun() ->  weight(I3Rtree, Pid, MyName, Next) end),
      receive
        {myPrevious, Pid, NewNext} ->

          Pid ! {myPrevious, {InitialX, InitialY}, {FinalX, FinalY}, MyName},
          Nnext = NewNext,

          receive
            {ok, {GNewInitialX, GNewInitialY}, {GNewFinalX, GNewFinalY} } ->
              {NewInitialX, NewInitialY} = {GNewInitialX, GNewInitialY},
              {NewFinalX, NewFinalY} = {GNewFinalX, GNewFinalY}
          end;

        {myNext, Pid} ->

          Pid ! {myNext, {InitialX, InitialY}, {FinalX, FinalY}, MyName},
          Nnext = Next,

          receive
            {ok, {GNewInitialX, GNewInitialY}, {GNewFinalX, GNewFinalY} } ->
              {NewInitialX, NewInitialY} = {GNewInitialX, GNewInitialY},
              {NewFinalX, NewFinalY} = {GNewFinalX, GNewFinalY}
          end;

        ok ->
          {NewInitialX, NewInitialY} = {InitialX, InitialY},
          {NewFinalX, NewFinalY} = {FinalX, FinalY},
          Nnext = Next
      end,
      server(MyName, Peers, Nnext, I3Rtree, {NewInitialX, NewInitialY}, {NewFinalX, NewFinalY}, {MaxRangeX, MaxRangeY});

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
          io:format("out of bound ~n"),
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

weight(I3Rtree, Pid, MyName, Next) ->
  Weight = i3RTree:weight(I3Rtree),
  Pid ! {weightResult, MyName, Next, Weight}.


timeNow() ->
  {H, M, S} = erlang:time(),
  H * 3600 + M * 60 + S.
