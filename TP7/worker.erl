-module(worker).
-export([start/6]).

start(Name, Group, State, Id, Sleep, Jitter) ->
	spawn(fun() -> init(Name, Group, State, Id, Sleep, Jitter) end).
% El group no lo recibe aca
% Debe recibir el manager

init(Name, Group, State, Id, Sleep, Jitter) ->
    Gui = gui:start(Name),
		% Cuando hace init, tiene que enviarle a su manager self y este eventualmente le contestara con su grupo de trabajo. Donde el mismo tambien esta.
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
	% por tanto segundos no puede enviar nada. Sleep (pero ojo, podria estar recibiendo. Fijate en el tp anterior se hace esto)
	% luego, debe enviar un mensaje random entre 1 y 20 a todos sus pares
	% pasa a escuchar. Cuando llega su mensaje hace recursion actualizando el gui y vuelve a espera para volver a enviar.
	% pd : si tenes alguna duda no me molesta, pregunta. hoy cuando llegue hacemos call y vemos que cosas seguir mejorando ;)
	% pdd : espero haber sido claro. Si no lo podes resolver yo lo hago, me importa mas que entiendas que esta pasando.
	% pdd: si te trabas lee el libro ;)
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
