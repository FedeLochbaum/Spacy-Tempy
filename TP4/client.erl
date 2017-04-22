-module(client).
-export([open/1,read/2,write/2,commit/1,abort/0]).

open(Server) ->
    Server ! {open, self()},
    receive
        {transaction, Validator, Store} ->
            register(handler,handler:start(self(), Validator, Store))
    end.


read(Ref,N) ->
    handler ! {read, Ref, N},
    receive
        {Ref, Value} ->
            io:format("The value is: ~p",[Value]),
            ok;
        _ ->
            abort
    end.

write(N,Value) ->
    handler ! {write, N, Value}.


commit(Ref) ->
    handler ! {commit, Ref},
    receive
        {Ref, ok} ->
            ok;
        {Ref, abort} ->
            abort
    end.

abort() ->
  handler ! abort.
