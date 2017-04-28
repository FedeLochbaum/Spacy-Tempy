-module(client).
-export([open/1,read/3,write/3,commit/2,abort/1]).

open(Server) ->
    Server ! {open, self()},
    receive
        {transaction, Validator, Store} ->
            handler:start(self(), Validator, Store)
    end.


read(Handler,N,Pid) ->
    Ref = 1,
    Handler ! {read, Ref, N},
    receive
        {Ref, Value} ->
            Pid ! {ok, Handler};
        _ ->
            abort
    end.

write(Handler,N,Value) ->
    Handler ! {write, N, Value}.


commit(Handler,Ref) ->
    Handler ! {commit, Ref},
    receive
        {Ref, ok} ->
            ok;
        {Ref, abort} ->
            abort
    end.

abort(Handler) ->
  Handler ! abort.
