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
				worker(Name,Gui, State, Sleep, MultiCast)
		end.


worker(Name,Gui, State, Sleep, Cast) ->
    receive
        {msg, Msg} ->
						io:format("soy el worker y me llego este mensaje: ~w ~p~n", [Name,Msg]),
						NTuple = color_change(Msg, State),
						io:format("soy el worker y mi tupla es: ~w ~p~n", [Name,NTuple]),
						Gui ! {color, NTuple},
            worker(Name, Gui, NTuple, Sleep, Cast);
        stop ->
            Gui ! stop
        after Sleep ->
            Message = rand:uniform(20),
            io:format("soy el worker y envio este mensaje: ~w ~p~n", [Name, Message]),
						Cast ! {send, Message},   % spawn(fun() -> sendMessage(Workers, Message) end),
            receiveMsg(Name, Message, Gui, State,Sleep, Cast)
    end.

receiveMsg(Name, Message, Gui, State,Sleep, Cast) ->
    receive
        {msg, Msg} ->
						io:format("soy el worker y me llego este mensaje: ~w ~p~n", [Name,Msg]),
						NTuple = color_change(Msg, State),
						io:format("Soy el worker mi tupla es: ~w ~p~n", [Name,NTuple]),
            Gui ! {color, NTuple},
						case Msg == Message of
							true -> worker(Name, Gui, NTuple, Sleep, Cast);
							false -> receiveMsg(Name, Message, Gui, NTuple,Sleep, Cast)
						end;
				Bla ->
						io:format("llego otra cosa: ~w~n", [Bla])
    end.


color_change(N, {R,G,B}) ->
    {G, B, ((R+N) rem 256)}.
