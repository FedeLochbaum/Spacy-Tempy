-module(entry).
-export([new/0,add/3,lookup/2,remove/2]).


new() ->
  maps:from_list([]).

add(Name,Entry,Entries) ->
  maps:put(Name,Entry,Entries).

remove(Name,Entries) ->
  maps:remove(Name,Entries).

lookup(Name,Entries) ->
  case maps:find(Name,Entries) of
    error ->
      unknown;
    {ok,Response} ->
      Response
  end.
