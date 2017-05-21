-module(worker).
-export([start/6]).

start(Name, Group, State, Id, Sleep, Jitter) ->
	spawn(fun() -> init(Name, Group, State, Id, Sleep, Jitter) end).


init(Name, Group, State, Id, Sleep, Jitter) ->
    Gui = gui:start(Name),
    %Cast  
    %Group 
	receive
	 	{deliver, Color, Peers} ->
            Cast ! {peers, Peers},
            Gui ! {color, Color},
            cast_change(Id, Cast, Sleep),
            worker(Id, Cast, Color, Gui, Sleep),
	end.


worker(Id, Cast, Color, Gui, Sleep) ->
	receive
        {deliver, {Wkr, N}} ->
            Other_Color = color_change(N, Color),
            Gui ! {color, Other_Color},
            if
                Wkr == Id ->
                    cast_change(Id, Cast, Sleep);
                true ->
                    ok
            end,
            worker(Id, Cast, Other_Color, Gui, Sleep);
        stop ->
            ok
    end.


color_change(N, {R,G,B}) ->
    {G, B, ((R+N) rem 256)}.


cast_change(Id, Cast, Sleep) ->
    Msg = {Id, rand:uniform(100)}.
