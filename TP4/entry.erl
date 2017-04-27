-module(entry).
-export([new/1, init/1, entry/2]).

new(Value) ->
	spawn_link(fun() -> init(Value) end).

init(Value) ->
	entry(Value, make_ref()).

entry(Value, Time) ->
   receive
	{read, Ref, Handler} ->
	   Handler ! {Ref, self(), Value, Time},
	   entry(Value, Time);
	{check, Ref, Read, Handler} ->
	   if
	      Read == Time ->
		 Handler ! {Ref, ok};
	      true ->
		 Handler ! {Ref, abort}
	   end,
	   entry(Value, Time);
  {write, New} ->
		NewRef = make_ref(),
		io:format("The reference is: ~p",[NewRef]),
		entry(New, NewRef);
   stop ->
		 ok
  end.
