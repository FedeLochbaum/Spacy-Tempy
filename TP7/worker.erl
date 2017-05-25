-module(worker).
-export([start/4 , startManager/1,stop/1]).

start(Name, Manager, Sleep,Jitter) ->
	register(Name, spawn(fun() -> init(Name, Manager, {0,0,0}, Sleep,Jitter) end)).

startManager(Time) ->
    register(manager, spawn(fun() -> manager([], Time) end)).

stop(Worker) ->
  Worker ! stop.

manager(List, Time) ->
	   receive
	       {subscribe, Worker, Jitter} ->
					 	 MultiCast = multicast:start(Jitter,Worker),
	           NewList = [MultiCast | List],
						 Worker ! {cast, MultiCast},
	           manager(NewList, Time)
	       after Time ->
					 	lists:map(fun(Casting) ->
							Casting ! {nodes, List}
						end, List)
	   end.

init(Name, Manager, State, Sleep,Jitter) ->
    Gui = gui:start(Name),
    Manager ! {subscribe, Name, Jitter},
		receive
			{cast, MultiCast} ->
				worker(Gui, State, Sleep, MultiCast)
		end.


worker(Gui, State, Sleep, Cast) ->
    receive
        {msg, Msg} ->
						Ntuple = color_change(Msg, State),
						Gui ! {color, Ntuple},
            worker(Gui, Ntuple, Sleep, Cast);
        stop ->
            ok
        after Sleep ->
            Message = rand:uniform(20),
            io:format("envio mensaje: ~w~n", [Message]),
						Cast ! {send, Message},   % spawn(fun() -> sendMessage(Workers, Message) end),
            receiveMsg(Message, Gui, State,Sleep, Cast)
    end.

receiveMsg(Message, Gui, State,Sleep, Cast) ->
    receive
        {msg, Msg} ->
			io:format("soy worker y me llego este mensaje: ~w~n", [Message]),
						Ntuple = color_change(Msg, State),
            Gui ! {color, Ntuple},
						case Msg == Message of
							true -> worker(Gui, State, Sleep, Cast);
							false -> receiveMsg(Message, Gui, Ntuple,Sleep, Cast)
						end;
				Bla ->
						io:format("llego otra cosa: ~w~n", [Bla])
    end.


color_change(N, {R,G,B}) ->
    {G, B, ((R+N) rem 256)}.
