-module(client).
-export([open/1,read/3,write/2,commit/1,abort/0]).

open(Server) ->
    Server ! {open, self()},
    receive
        {transaction, Validator, Store} ->
            register(handler,handler:start(self(), Validator, Store))
    end.


read(Ref,N,Pid) ->
    handler ! {read, Ref, N},
    receive
        {Ref, Value} ->
            Pid ! ok;
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
