-module(worker).
-export([start/4 , startManager/2]).

start(Name, Manager, Sleep, Cast) ->
	register(Name, spawn(fun() -> init(Name, Manager, {0,0,0}, Sleep, Cast) end)).


init(Name, Manager, State, Sleep, Cast) ->
    Gui = gui:start(Name),
    Manager ! {subscribe, Name},
    worker(Cast, Gui, State, Sleep).
		% Cuando hace init, tiene que enviarle a su manager self y este eventualmente le contestara con su grupo de trabajo. Donde el mismo tambien esta.


worker(Cast, Gui, State, Sleep) ->
	% pasa a escuchar. Cuando llega su mensaje hace recursion actualizando el gui y vuelve a espera para volver a enviar.
    receive
        {msg, Msg} -> 
            Gui ! {color, color_change(Msg, State)},
            worker(Cast, Gui, State, Sleep);
        stop ->
            ok
        after Sleep ->
            Message = rand:uniform(20),
            io:format("envío mensaje: ~w~n", [Message]),
            Cast ! {msg, Message},
            receiveMsg(Message, Gui, State),
            worker(Cast, Gui, State, Sleep)
    end.


receiveMsg(Message, Gui, State) ->  
    receive
        {msg, Msg} ->    
            Gui ! {color, color_change(Msg, State)}, 
            receiveMsg(Message, Gui, State);    
        {msgOwner, Message} ->      
            Gui ! {color, color_change(Message, State)},  
            ok  
    end.
    

manager(Cast, List, Time) ->
    receive
        {subscribe, Pid} ->
            NewList = [Pid]++List,
            manager(Cast, NewList, Time)
        after Time ->
            Cast ! {subscribe, List}
    end.


startManager(Cast, Time) ->
    register(manager, spawn(fun() -> manager(Cast, [], Time) end)).


color_change(N, {R,G,B}) ->
    {G, B, ((R+N) rem 256)}.