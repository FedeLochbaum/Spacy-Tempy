- module(i3RTree).
- export([new/0, subscribe/4, unsubscribe/2, move/4, timelapse_query/3, interval_query/3, event_query/2, track_query/2]).


new() ->
  {rstar:new(3),maps:new()}.

subscribe(Pid, {X, Y}, Instant, {Rtree, Map}) ->
  % NOT IMPLEMENTED
  {Rtree, Map}.

unsubscribe(Pid, {Rtree, Map}) ->
  % NOT IMPLEMENTED
  {Rtree, Map}.

move(Pid, {X,Y}, Instant, {Rtree, Map}) ->
  % NOT IMPLEMENTED
  {Rtree, Map}.

timelapse_query(Region, Instant, {Rtree, Map}) ->
  % NOT IMPLEMENTED
  [].

interval_query(Region, {Ti,Tk}, {Rtree, Map}) ->
  % NOT IMPLEMENTED
  [].

event_query(Region, {Rtree, Map}) ->
  % NOT IMPLEMENTED
  [].

track_query(Pid, {Rtree, Map}) ->
  % NOT IMPLEMENTED
  [].
