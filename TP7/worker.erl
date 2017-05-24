-module(worker).
-export([start/4 , startManager/2,stop/1]).

start(Name, Manager, Sleep, Cast) ->
	register(Name, spawn(fun() -> init(Name, Manager, {0,0,0}, Sleep, Cast) end)).

startManager(Cast, Time) ->
    register(manager, spawn(fun() -> manager(Cast, [], Time) end)).

stop(Worker) ->
  Worker ! stop.

manager(Cast, List, Time) ->
	   receive
	       {subscribe, Pid} ->
	           NewList = [Pid]++List,
	           manager(Cast, NewList, Time)
	       after Time ->
	           Cast ! {subscribe, List}
	   end.

init(Name, Manager, State, Sleep, Cast) ->
    Gui = gui:start(Name),
    Manager ! {subscribe, Name},
    worker(Cast, Gui, State, Sleep).
		% Cuando hace init, tiene que enviarle a su manager self y este eventualmente le contestara con su grupo de trabajo. Donde el mismo tambien esta.


worker(Cast, Gui, State, Sleep) ->
	% pasa a escuchar. Cuando llega su mensaje hace recursion actualizando el gui y vuelve a espera para volver a enviar.
    receive
        {msg, Msg} ->
						Ntuple = color_change(Msg, State),
						Gui ! {color, Ntuple},
            worker(Cast, Gui, Ntuple, Sleep);
        stop ->
            ok
        after Sleep ->
            Message = rand:uniform(20),
            io:format("envio mensaje: ~w~n", [Message]),
            Cast ! {msg, Message},
            receiveMsg(Message, Gui, State,Cast,Sleep)
    end.


receiveMsg(Message, Gui, State,Cast,Sleep) ->
    receive
        {msg, Msg} ->
			io:format("soy worker y me llego este mensaje: ~w~n", [Message]),
						Ntuple = color_change(Msg, State),
            Gui ! {color, Ntuple},
						case Msg == Message of
							true -> worker(Cast, Gui, State, Sleep);
							false -> receiveMsg(Message, Gui, Ntuple,Cast,Sleep)
						end;
				Bla ->
						io:format("llego otra cosa: ~w~n", [Bla])
    end.


color_change(N, {R,G,B}) ->
    {G, B, ((R+N) rem 256)}.
