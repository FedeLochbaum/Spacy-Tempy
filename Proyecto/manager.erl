-module(manager).
-export([start/2, newManager/3, newManager/4]).

start({Name, Node}, TimeLapse) ->
  register(Name, spawn(fun() -> initManager({Name, Node}, TimeLapse) end)).

newManager(Name, Managers, TimeLapse) ->
  register(Name, spawn(fun() -> initNewManager(Name, Managers, TimeLapse) end)).

newManager(Name, From, Managers, TimeLapse) ->
  register(Name, spawn(fun() -> initNewManager({Name,From}, Managers, TimeLapse) end)).

initNewManager({Name, Node}, Managers, TimeLapse) ->
  Manager = lists:nth(rand:uniform(length(Managers)), Managers), % selecciono un manager random
  Manager ! {newManager, {Name, Node}},
  receive
    {yourNewServer, Pid, {NameServer, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY}, {LoadBalancing,LoadBalancing}}, Peers, {ToMonitor, StateMonitor}} ->  % aca deberiamos pasarle todo lo que tiene la funcion server (obviamente con su estado)
      createNewServer(NameServer, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY}, {LoadBalancing,LoadBalancing}, Node), % asumo que aca podre ver el nombre del server.
      Monitor = monitor(process, ToMonitor),
      lists:map(fun(Peer) -> Peer ! ok end, Peers),
      manager(Name, [NameServer], Managers, {ToMonitor, StateMonitor, Monitor}, TimeLapse, Pid)
  end.


createNewServer({NameServer, NodeServer}, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY}, {LoadBalancing,LoadBalancing}, Node) ->
io:format("Selected Server is ~w~n", [{NameServer, Peers, Next, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY}, {LoadBalancing,LoadBalancing}}]),
register(NameServer,
            spawn(fun() ->
              distributedServer:server({NameServer, Node}, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY}, {LoadBalancing,LoadBalancing})
            end)).



initManager(Name, TimeLapse) ->
  receive
    {servers, Servers} ->
      waitForPeers(Name, Servers, TimeLapse)
  end.

waitForPeers(Name, Servers, TimeLapse) ->
  receive
    {managers, Managers,  {monitors, Observable, Observer}} -> % obsever = a mi, observable, a quien observo yo
      Monitor = monitor(process, Observable),
      manager(Name, Servers, Managers, {Observable, 0 ,Monitor}, TimeLapse, Observer)
  end.

getWeight(Peers) ->
  lists:map(fun(Peer) -> Peer ! {weight, self()} end, Peers),
  receiveReplies(length(Peers), []).

receiveReplies(0, Replies) ->
  Replies;

receiveReplies(Peers, Replies) ->
  receive
    {weightResult, Pid, Server, Weight} ->
      Res = Replies ++ [{Pid, Server, Weight}];
    _ ->
      Res = Replies
  end,
  receiveReplies(Peers -1, Res).

maxWeight(Replies) ->
F = fun({Manager, Server, Weight}, {ManagerMax, ServerMax, WeightMax}) ->
    if
      Weight >= WeightMax ->
        Res = {Manager, Server, Weight};
      true ->
        Res = {ManagerMax, ServerMax, WeightMax}
    end,
    Res
  end,
 lists:foldl(F, {0,0,0}, Replies).


manager({Name, Node}, Servers, Managers, {Observable, StateMonitor, Monitor}, TimeLapse, Observer) ->
  receive
    {newObserver, MyNewObserver} ->
      manager({Name, Node}, Servers, Managers, {Observable, StateMonitor, Monitor}, TimeLapse, MyNewObserver);

    {updateState, States} ->
      io:format("el nuevo estado es ~w~n", [States]),
      manager({Name, Node}, Servers, Managers, {Observable, States, Monitor}, TimeLapse, Observer);

    {newManager, NewManager} ->
      spawn(
            fun() ->
              Replies = getWeight(Managers ++ [{Name, Node}]),
              {Manager, Server, Weight} = maxWeight(Replies),
              Manager ! {getServer, Server, NewManager}
      end),
      manager({Name, Node}, Servers, Managers, {Observable, StateMonitor, Monitor}, TimeLapse, Observer);

    {weight, Pid} ->
      Replies = distributedServer:getWeight(Servers),
      lists:map(fun(S) -> S ! ok end, Servers),
      {S,_, Weight} = distributedServer:maxWeight(Replies),
      Pid ! {weightResult, Name, S, Weight},
      manager({Name, Node}, Servers, Managers, {Observable, StateMonitor, Monitor}, TimeLapse, Observer);

    {getServer, Server, NewManager} ->

      case length(Servers) > 1 of
        true ->
          Server ! {state, Name};
        false ->
          Server ! partition,
          Server ! {state, Name}
      end,

      receive
        {state, State} ->
          Server ! {stopServerForClone, {Name, Node}},
           receive
             {ok, Peers} ->
               timer:sleep(2000),
               Server ! stop,
               demonitor(Monitor),
               NewManager ! {yourNewServer, {Name, Node}, State, Peers, {Observable, StateMonitor}},
               manager({Name, Node}, lists:subtract(Servers, [Server]), Managers, {NewManager, 0 ,monitor(process, NewManager)}, TimeLapse, Observer)
          end
      end;
    {'DOWN', Monitor, process, Object, Info} ->
          io:format("el stateMonitor es ~w~n", [StateMonitor]),
          {{ObservableOfObservable, StateMonitorOfObservable, MonitorOfObservable}, StatesServersObservable } = StateMonitor,
          demonitor(Monitor),
          % demonitor(MonitorOfObservable), % por si acaso.
          NewMonitor = monitor(process, ObservableOfObservable),
          ObservableOfObservable ! {newObserver, {Name, Node}},
          % levantar los servers y agregarlos a mi lista de servers
          NamesServer = createsServersOfStates(StatesServersObservable, Node),

          manager({Name, Node}, Servers ++ NamesServer, Managers, {ObservableOfObservable, StateMonitorOfObservable, NewMonitor}, TimeLapse, Observer)
    after
      TimeLapse ->
        StatesServers = getStates({Name, Node}, Servers),
        io:format("send state to ~w~n", [Observer]),
        Observer ! {updateState, {{Observable, StateMonitor, Monitor} ,StatesServers}},
        manager({Name, Node}, Servers, Managers, {Observable, StateMonitor, Monitor}, TimeLapse, Observer)

  end.



getStates(Name, Servers) ->
  lists:map(fun(S) -> S ! {state, Name} end, Servers),
  receiveStates(length(Servers), []).

receiveStates(0, States) ->
  States;

receiveStates(N, States) ->
  receive
    {state, State} ->
      receiveStates(N-1, States ++ [State])
  end.


createsServersOfStates(StatesServersObservable, Node) ->
  F = fun({{NameServer, NodeServer}, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY}, {LoadBalancing,LoadBalancing}}, Current) ->
        createNewServer({NameServer, NodeServer}, Peers, Next, I3Rtree, {InitialX, InitialY}, {FinalX, FinalY}, {MaxRangeX, MaxRangeY}, {LoadBalancing,LoadBalancing}, Node),
        Current ++ NameServer
      end,
  NameServers = lists:foldl(F, [], StatesServersObservable),
  NameServers.
