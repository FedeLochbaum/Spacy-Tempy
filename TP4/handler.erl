-module(handler).
-export([start/3]).

start(Client, Validator, Store) ->
   spawn_link(fun() -> init(Client, Validator, Store) end).

init(Client, Validator, Store) ->
   handler(Client, Validator, Store, [], []).

handler(Client, Validator, Store, Reads, Writes) ->
   receive
	{read, Ref, N} ->
	    case lists:keysearch(N, 1, Writes) of
	       {value, {N, _, Value}} ->
		   value 
		   handler(Client, Validator, Store, Reads, Writes);
 	       false ->
 	           element(N, Store) !  
		   :
		   handler(Client, Validator, Store, Reads, Writes)
	    end;
	{Ref, Entry, Value, Time} ->
	    :
	    handler(Client, Validator, Store, [...|Reads], Writes);
	{write, N, Value} ->
	    Added = [{N, ..., ...}|...],
	    handler(Client, Validator, Store, Reads, Added);
	{commit, Ref} ->
	    Validator ! {validate, Ref, Reads, Writes, Client};
	abort -> ok
   end.