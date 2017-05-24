-module(worker).
-export([start/3 ,stop/1, peers/2]).

start(Name, Sleep, Cast) ->
	register(Name, spawn(fun() -> init(Name, {0,0,0}, Sleep, Cast) end)).


stop(Worker) ->
  Worker ! stop.

init(Name, State, Sleep, Cast) ->
    Gui = gui:start(Name),
    worker(Cast, Gui, State, Sleep).
		% Cuando hace init, tiene que enviarle a su manager self y este eventualmente le contestara con su grupo de trabajo. Donde el mismo tambien esta.

peers(Workers, Cast) ->
    Cast ! {subscribe, Workers}.

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
            io:format("envio mensaje: ~w~n", [Message]),
            Cast ! {msg, Message},
            receiveMsg(Message, Gui, State,Cast,Sleep)
    end.


receiveMsg(Message, Gui, State,Cast,Sleep) ->
    receive
        {msg, Msg} ->
			io:format("soy worker y me llego este mensaje: ~w~n", [Message]),
            Gui ! {color, color_change(Msg, State)},
						case Msg == Message of
							true -> worker(Cast, Gui, State, Sleep);
							false -> receiveMsg(Message, Gui, State,Cast,Sleep)
						end;
				Bla ->
						io:format("llego otra cosa: ~w~n", [Bla])
    end.


color_change(N, {R,G,B}) ->
    {G, B, ((R+N) rem 256)}.
