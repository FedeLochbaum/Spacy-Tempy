-module(server).
-export([start/0, start/2, stop/0, init/0, init/2]).

start() ->
    register(server, spawn(server, init, [])).

start(Domain, DNS) ->
    register(server, spawn(server, init, [Domain, DNS])).

stop() ->
    server ! stop,
    unregister(server).

init() ->
    server(entry:new(), 0).

init(Domain, Parent) ->
    Parent ! {register, Domain, {dns, self()}},
    server(entry:new(), 0).

server(Entries, TTL) ->
    receive{request, From, Req} ->
        io:format("request ~w~n", [Req]),
        Reply = entry:lookup(Req, Entries),
        From ! {reply, Reply, TTL},
        server(Entries, TTL);
    {register, Name, Entry} ->
        io:format("register ~w~n", [Name]),
        Updated = entry:add(Name, Entry, Entries),
        server(Updated, TTL);
    {deregister, Name} ->
        io:format("deregister ~w~n", [Name]),
        Updated = entry:remove(Name, Entries),
        server(Updated, TTL);
    {ttl, Sec} ->
        server(Entries, Sec);
    status ->
        io:format("cache ~w~n", [Entries]),
        server(Entries, TTL);
    stop ->
        io:format("closing down~n", []),
        ok;
    Error ->
        io:format("strange message ~w~n", [Error]),
        server(Entries, TTL)
end.
