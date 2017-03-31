-module(rudy).
-export([init/2,handler/1,request/1,reply/1,start/2, stop/0,registers_handlers/3,throwHandler/2,abortPids/1,abort/0]).
-import(http,[parse_request/1,ok/1]).

-define(Pids, []).


start(Port,NUM) ->
  register(rudy, spawn(fun() -> init(Port,NUM) end)).

stop() ->
  rudy ! close.
  %exit(whereis(rudy), "time to die").

abort() -> receive
  close -> abortPids(?Pids)
end.


abortPids ([]) ->
  ok;
abortPids([X |XS]) ->
  exit(whereis(X), "time to die"),
  abortPids(XS).


init(Port,NUM) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  registers_handlers(Port,Opt,NUM).

registers_handlers(_,_,0) ->
  ok;

registers_handlers(Port,Opt,NUM) ->
  Pid = spawn(rudy,throwHandler,[Port,Opt]),
  lists:append(?Pids,Pid),
  registers_handlers(Port,Opt,NUM-1).

throwHandler(Port,Opt) ->
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      handler(Listen),
      gen_tcp:close(Listen),
      ok;
    {error, _} ->
      error
  end.

handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      request(Client);
    {error, _} ->
      error
  end,
  handler(Listen).


request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
        Request = http:parse_request(Str),
        Response = reply(Request),
        gen_tcp:send(Client, Response);
    {error, Error} ->
        io:format("rudy: error: ~w~n", [Error])
  end,
  gen_tcp:close(Client).


reply({{get, URI, _}, _, _}) ->
  http:ok(URI).
