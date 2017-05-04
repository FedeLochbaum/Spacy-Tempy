-module(resolver).
-export([start/1, stop/0, init/1]).

start(Root) ->
    register(resolver, spawn(resolver, init, [Root])).

stop() ->
    resolver ! stop,
    unregister(resolver).

init(Root) ->
    Empty = cache:new(),
    Inf = time:inf(),
    Cache = cache:add([], Inf, {dns, Root}, Empty),
    resolver(Cache).

resolver(Cache) ->
    receive
      {request, From, Req} ->
          io:format("request ~w ~w~n", [From,Req]),
          {Reply, Updated} = resolve(Req, Cache),
          From ! {reply, Reply},
          resolver(Updated);
      status ->
        io:format("cache ~w~n", [Cache]),
        resolver(Cache);
      stop ->
        io:format("closing down~n", []),
        ok;
      Error ->
        io:format("strange message ~w~n", [Error]),
        resolver(Cache)
    end.

resolve(Name, Cache)->
    io:format("resolve ~w ", [Name]),
    case cache:lookup(Name, Cache) of
      unknown ->
          io:format("unknown ~n ", []),
          recursive(Name, Cache);
      invalid ->
          io:format("invalid ~n ", []),
          recursive(Name, cache:remove(Name, Cache));
      {ok, Reply} ->
          io:format("found ~w ~n ", [Reply]),
          {Reply, Cache}
    end.

recursive([Name|Domain], Cache) ->
    io:format("recursive ~w ", [Domain]),
    case resolve(Domain, Cache) of
      {unknown, Updated} ->
          io:format("unknown ~n", []),
          {unknow, Updated};
      {{dns, Srv}, Updated} ->
          Srv ! {request, self(), Name},
          io:format("sent ~w request to ~w ~n", [Name, Srv]),
          receive
              {reply, Reply, TTL} ->
                  Expire = time:add(time:now(), TTL),
                  {Reply, cache:add([Name|Domain], Expire, Reply, Updated)}
          end
    end.
