-module(cache).
-export([new/0,add/4,lookup/2,remove/2]).

new() ->
  maps:from_list([]).

add([],_,_,Cache) ->
  Cache;

add([Name|Domain], Time, DNS, OldCache) ->
  add(Domain,Time,DNS,maps:put(Name,{DNS,Time},OldCache)).


isValid({_,Time}) ->
  time:valid(Time,time:now()).

lookup(Name,Cache) ->
  case maps:find(Name,Cache) of
    error ->
      unknown;
    {ok,Response} ->
      case isValid(Response) of
        true ->
          {ok,Response};
        false ->
          invalid
      end
    end.

remove(Name,Cache) ->
  maps:remove(Name,Cache).
