-module(server).
-export([start/1, stop/1]).

start(N) ->
    register(server,spawn(fun() -> init(N) end)).

stop(Server) ->
    Server ! stop.

init(N) ->
    Store = store:new(N),
    Validator = validator:start(),
    server(Validator, Store).

server(Validator, Store) ->
    receive
        {open, Client} ->
            Client ! {transaction, Validator, Store},
            server(Validator, Store);
        stop ->
            store:stop(Store)
    end.
