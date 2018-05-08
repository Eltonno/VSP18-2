-module(koordinator).
-export([start/0, init_coordinator/0, initial/1, twenty_percent_of/1, handle_briefterm/4, set_initial_mis/3]).

maybe_log_wrong_term(CmiReceived, MinimumCmi) when CmiReceived > MinimumCmi ->
  util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Error! Received terminated ggT is bigger than the smalles received ggT (" ++ integer_to_list(CmiReceived) ++ "," ++ integer_to_list(MinimumCmi) ++ ")\n");
maybe_log_wrong_term(_CmiReceived, _MinimumCmi) when true ->
  ok.

handle_briefterm(ActVal, CMi, ClientName, CZeit) ->
  Minimum = maps:get(smallestGgT, ActVal),
  if
    CMi > Minimum ->
      util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Trying to correct with sending " ++ integer_to_list(Minimum) ++ " to " ++ atom_to_list(ClientName) ++ " at " ++ vsutil:now2string(CZeit) ++ "\n"),
      maps:get(ClientName, maps:get(clientsToPID, ActVal)) ! {sendy, Minimum};
    true ->
      util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), atom_to_list(ClientName) ++ " reports correct termination with ggT " ++ CMi ++ " at " ++ vsutil:now2string(CZeit) ++ "\n")
  end.

prompt_all_ggt(_ClientsToPID, []) -> ok;
prompt_all_ggt(ClientsToPID, [Client | RestClients]) ->
  maps:get(Client, ClientsToPID) ! {self(), tellmi},
  receive
    {mi, Mi} -> util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "client: " ++ Client ++ " has mi: " ++ Mi ++ "\n")
  end,
  prompt_all_ggt(ClientsToPID, RestClients).

check_status_all_ggt(_ClientsToPID, []) -> ok;
check_status_all_ggt(ClientsToPID, [Client | RestClients]) ->
  {ClientName, ClientNode} = maps:get(Client, ClientsToPID),
  PingResponse = net_adm:ping(ClientNode),
  if
    PingResponse =:= pang ->
      util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "client node of " ++ Client ++ " is dead\n");
    true ->
      {ClientName, ClientNode} ! {self(), pingGGT},
      receive
        {pongGGT, GgTName} -> util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "ggT-Process: " ++ GgTName ++ " is alive\n")
      end
  end,
  check_status_all_ggt(ClientsToPID, RestClients).

send_ys_to_ggTs([], [], _ClientsToPID) -> ok;
send_ys_to_ggTs([Mi | Tail], [Client | ClientTail], ClientsToPID) ->
  maps:get(Client, ClientsToPID) ! {sendy, Mi},
  send_ys_to_ggTs(Tail, ClientTail, ClientsToPID).

twenty_percent_of(Clients) ->
  EightyPercent = rounding(length(Clients) * 0.8),
  Length = length(lists:nthtail(EightyPercent, Clients)),
  if
    Length >= 2 -> lists:nthtail(EightyPercent, util:shuffle(Clients));
    true -> lists:nthtail(length(Clients)-2, util:shuffle(Clients))
  end.

rounding(X) when X < 0 ->
  trunc(X);
rounding(X) ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T + 1
  end.

set_initial_mis([], [], _ClientsToPID) -> ok;
set_initial_mis([Mi | Tail], [Client | ClientTail], ClientsToPID) ->
  maps:get(Client, ClientsToPID) ! {setpm, Mi},
  set_initial_mis(Tail, ClientTail, ClientsToPID).

-spec start_calculation(integer(), list(), map()) -> any().
start_calculation(WggT, Clients, ClientsToPID) ->
  util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Start calculation for ggT: " ++ integer_to_list(WggT) ++ "\n"),
  Mis = vsutil:bestimme_mis(WggT, length(Clients)),
  set_initial_mis(Mis, Clients, ClientsToPID),
  StartingClients = twenty_percent_of(Clients),
  StartYs = vsutil:bestimme_mis(WggT, length(StartingClients)),
  send_ys_to_ggTs(StartYs, StartingClients, ClientsToPID).

calculation(ActVal) ->
  receive
  % Starts the calculation
    {calc, WggT} ->
      start_calculation(WggT, maps:get(clients, ActVal), maps:get(clientsToPID, ActVal)),
      calculation(ActVal);
  % Toggles the correct flag
    toggle ->
      Config = maps:get(config, ActVal),
      {ok, CorrectFlag} = vsutil:get_config_value(korrigieren, Config),
      NewFlag = (CorrectFlag + 1) rem 2,
      UpdatedActVal = maps:update(config, lists:keyreplace(korrigieren, 1, Config, {korrigieren, NewFlag}), ActVal),
      util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Correct flag is now set to: " ++ NewFlag ++ " from: " ++ CorrectFlag ++ "\n"),
      calculation(UpdatedActVal);
  % Ask all ggTs current Mi
    prompt ->
      prompt_all_ggt(maps:get(clientsToPID, ActVal), maps:get(clients, ActVal)),
      calculation(ActVal);
  % Pings all ggTs
    nudge ->
      check_status_all_ggt(maps:get(clientsToPID, ActVal), maps:get(clients, ActVal)),
      calculation(ActVal);
    kill ->
      shutdown(ActVal);
  % ggT process signals its mi
    {briefmi, {ClientName, CMi, CZeit}} ->
      util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), atom_to_list(ClientName) ++ " reports new Mi " ++ integer_to_list(CMi) ++ " at " ++ vsutil:now2string(CZeit) ++ "\n"),
      UpdatedMinimumActVal = maps:update(smallestGgT, min(CMi, maps:get(smallestGgT, ActVal)), ActVal),
      calculation(UpdatedMinimumActVal);
  % ggT process is done
    {From, briefterm, {ClientName, CMi, CTermZeit}} ->
      Config = maps:get(config, ActVal),
      {ok, CorrectFlag} = vsutil:get_config_value(korrigieren, Config),
      maybe_log_wrong_term(CMi, maps:get(smallestGgT, ActVal)),
      if
        CorrectFlag =:= true ->
          handle_briefterm(ActVal, CMi, From, CTermZeit);
        true ->
          util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), atom_to_list(ClientName) ++ " reports termination with ggT " ++ util:to_String(CMi) ++ " at " ++ vsutil:now2string(CTermZeit) ++ "\n")
      end,
      calculation(ActVal)
  end.

set_neighbors(ClientsToPID, [Middle, Last], [First, Second | _Tail]) ->
  util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Set neighbour for ggT-process " ++ atom_to_list(Last) ++ " with neighbours: " ++ atom_to_list(Middle) ++ " " ++ atom_to_list(First) ++ "\n"),
  maps:get(Last, ClientsToPID) ! {setneighbors, Middle, First},
  util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Set neighbour for ggT-process " ++ atom_to_list(First) ++ " with neighbours: " ++ atom_to_list(Last) ++ " " ++ atom_to_list(Second) ++ "\n"),
  maps:get(First, ClientsToPID) ! {setneighbors, Last, Second};

set_neighbors(ClientsToPID, [Left, Middle, Right | Tail], Clients) ->
  util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Set neighbour for ggT-process " ++ atom_to_list(Middle) ++ " with neighbours: " ++ atom_to_list(Left) ++ " " ++ atom_to_list(Right) ++ "\n"),
  maps:get(Middle, ClientsToPID) ! {setneighbors, Left, Right},
  set_neighbors(ClientsToPID, [Middle, Right] ++ Tail, Clients).

bind_ggT(NameService, GgTName, ClientsToPID) ->
  NameService ! {self(), {lookup, GgTName}},
  receive
    not_found ->
      util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Could not bind " ++ atom_to_list(GgTName) ++ "\n"),
      maps:put(GgTName, undefined, ClientsToPID);
    {pin, GgTPID} ->
      util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Bound ggT-process " ++ atom_to_list(GgTName) ++ "\n"),
      maps:put(GgTName, GgTPID, ClientsToPID)
  end.

bind_ggTs(ActVal) ->
  Config = maps:get(config, ActVal),
  {ok, NSNode} = vsutil:get_config_value(nameservicenode, Config),
  pong = net_adm:ping(NSNode),
  NameService = global:whereis_name(nameservice),
  ClientsToPID = lists:foldr(fun(GgTName, Acc) ->
    bind_ggT(NameService, GgTName, Acc) end, maps:get(clientsToPID, ActVal), maps:get(clients, ActVal)),
  maps:update(clientsToPID, ClientsToPID, ActVal).

kill_clients([]) -> exit(self(), normal), ok;
kill_clients([Client | Tail]) ->
  WhereIs = whereis(Client),
  case WhereIs of
    undefined -> ok;
    _Else -> Client ! kill
  end,
  kill_clients(Tail).

shutdown(ActVal) ->
  util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Shutting down " ++ integer_to_list(length(maps:get(clients, ActVal))) ++ " ggT-processes\n"),
  kill_clients(maps:get(clients, ActVal)),
  exit(self(), normal), ok.

-spec initial(map()) -> map().
initial(ActVal) ->
  receive
    {From, getsteeringval} ->
      Config = maps:get(config, ActVal),
      {ok, WorkingTime} = vsutil:get_config_value(arbeitszeit, Config),
      {ok, TerminationTime} = vsutil:get_config_value(termzeit, Config),
      {ok, QuotaPercentage} = vsutil:get_config_value(quote, Config),
      {ok, GGTProcessNumber} = vsutil:get_config_value(ggtprozessnummer, Config),
      Quota = max(2, round(length(maps:get(clients, ActVal)) * QuotaPercentage / 100)),
      From ! {steeringval, WorkingTime, TerminationTime, Quota, GGTProcessNumber},
      util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "getsteeringval: " ++ pid_to_list(From) ++ "\n"),
      initial(ActVal);
    {hello, ClientName} ->
      NewActVal = maps:update(clients, maps:get(clients, ActVal) ++ [ClientName], ActVal),
      util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "hello: " ++ atom_to_list(ClientName) ++ " #ofclients: " ++ integer_to_list(length(maps:get(clients, NewActVal))) ++ "\n"),
      initial(NewActVal);
    reset -> initial(maps:update(clients, [], ActVal));
    kill -> shutdown(ActVal);
    step -> Config = maps:get(config, ActVal),
      {ok, ExpectedClients} = vsutil:get_config_value(ggtprozessnummer, Config),
      ActualClients = length(maps:get(clients, ActVal)),
      util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Initial ActVal completed. Registered " ++ integer_to_list(ExpectedClients) ++ "/" ++ integer_to_list(ActualClients) ++ " ggT-processes\n"),
      util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Start building ring\n"),
      BoundClientsActVal = bind_ggTs(ActVal),
      % build ring
      ShuffledClients = util:shuffle(maps:get(clients, ActVal)),
      set_neighbors(maps:get(clientsToPID, BoundClientsActVal), ShuffledClients, ShuffledClients),
      util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Done building ring\n"),
      util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Switching to calculation ActVal\n"),
      calculation(BoundClientsActVal)
  end,
  ActVal.

init_coordinator() ->
  {ok, Config} = file:consult("koordinator.cfg"),
  util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "starttime: " ++ util:timeMilliSecond() ++ " | mit PID " ++ pid_to_list(self()) ++ "\n"),
  util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "koordinator.cfg geoeffnet...\n"),
  {ok, CoordName} = vsutil:get_config_value(koordinatorname, Config),
  util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "koordinator.cfg gelesen...\n"),
  {ok, NSNode} = vsutil:get_config_value(nameservicenode, Config),
%%  {match, [NSName]} = re:run(util:to_String(NSNode), "[a-z0-9]+", [{capture, first, list}]),
  pong = net_adm:ping(NSNode),
  NameService = global:whereis_name(nameservice),
  util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "Nameservice " ++ pid_to_list(NameService) ++ " gebunden...\n"),
  erlang:register(CoordName, self()),
  util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "lokal registriert...\n"),
  NameService ! {self(), {rebind, CoordName, node()}},
  receive
    ok -> util:logging(list_to_atom("Koordinator@" ++ atom_to_list(node()) ++ ".log"), "beim Namensdienst registriert.\n")
  end,
  initial(#{config => Config, clients => [], clientsToPID => #{}, smallestGgT => 134217728}).

start() ->
  spawn(?MODULE, init_coordinator, []).
