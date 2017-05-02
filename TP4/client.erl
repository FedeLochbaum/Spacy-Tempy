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
        {Ref, _} ->
            Pid ! {ok, Handler};
        _ ->
            Pid ! {abort, Handler},
            abort
    end.

write(Handler,N,Value, Pid) ->
    Handler ! {write, N, Value},
    Pid ! {ok, Handler}.


commit(Handler,Ref,Pid) ->
    Handler ! {commit, Ref},
    receive
        {Ref, ok} ->
            Pid ! {ok, Handler};
        {Ref, abort} ->
            Pid ! {abort, Handler},
            abort
    end.

abort(Handler) ->
  Handler ! abort.
