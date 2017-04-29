-module(client).
-export([open/1,read/3,write/4,commit/3,abort/1]).

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

write(Handler,N,Value, Pid) ->
    Handler ! {write, N, Value},
    receive
        {N, Value} ->
            Pid ! {ok, Handler};
        _ ->
            abort
    end.


commit(Handler,Ref,Pid) ->
    Handler ! {commit, Ref},
    receive
        {Ref, ok} ->
            Pid ! {ok, Handler};
        {Ref, abort} ->
            Pid ! abort
    end.

abort(Handler) ->
  Handler ! abort.
