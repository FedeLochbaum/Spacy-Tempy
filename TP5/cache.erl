-module(cache).
-export([new/0,add/4,lookup/2,remove/2]).

new() ->
  maps:from_list([]).

% add([],_,_,Cache) ->
%   Cache;
%
% add([Name|Domain], Time, DNS, OldCache) ->
%   add(Domain,Time,DNS,maps:put(Name,{DNS,Time},OldCache)).

add(ListDomain, Time, DNS, OldCache) ->
  Cache = maps:put(ListDomain,{DNS,Time},OldCache),
  io:format("La cache es : ~w~n", [Cache]),
  Cache.

isValid({_,Time}) ->
  time:valid(Time,time:now()).

lookup(Name,Cache) ->
  case maps:find(Name,Cache) of
    error ->
      unknown;
    {ok,{DNS,Time}} ->
      case isValid({DNS,Time}) of
        true ->
          io:format("El dns encontrado es : ~w~n", [DNS]),
          {ok,DNS};
        false ->
          invalid
      end
    end.

remove(Name,Cache) ->
  maps:remove(Name,Cache).
