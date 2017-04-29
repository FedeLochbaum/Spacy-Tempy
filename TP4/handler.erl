-module(handler).
-export([start/3]).
-import(store,[lookup/2]).

start(Client, Validator, Store) ->
   spawn_link(fun() -> init(Client, Validator, Store) end).

init(Client, Validator, Store) ->
   handler(Client, Validator, Store, [], []).

handler(Client, Validator, Store, Reads, Writes) ->
  receive
   {read, Ref, N} ->
      case lists:keysearch(N, 1, Writes) of
          {value, {N, _, Value}} ->
              % si fue escrita no debe hacer nada
              Client ! {Ref, Value}, % otra posible opcion, el enunciado no es claro con respecto a este caso
              handler(Client, Validator, Store, Reads, Writes);
          false ->
              Entry = lookup(N,Store), %si no se escribio pido la Nesima entrada
              Entry ! {read, Ref, self()}, % le envio el mensaje read, pasandole la referencia y el handler(self)
              handler(Client, Validator, Store, Reads, Writes)
      end;
  {Ref, Entry, Value, Time} ->
      Client ! {Ref, Value}, % le envio al cliente la resputa,
      handler(Client, Validator, Store, [{Entry,Time}|Reads], Writes); % agrego la tupla a reads
  {write, N, Value} ->
      Added = [{N,lookup(N,Store),Value}|Writes], % agrego a writes la tupla de escritura local
      Client ! {N, Value},
      handler(Client, Validator, Store, Reads, Added);
  {commit, Ref} ->
      Validator ! {validate, Ref, Reads, Writes, Client};
  abort -> ok
 end.
