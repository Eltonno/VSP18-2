-module(koordinator).
-export([start/0, init_coordinator/0, initial/1, twenty_percent_of/1, handle_briefterm/4, set_initial_mis/3]).

log(Message) ->
  Logfile = list_to_atom(lists:concat(["Koordinator@", atom_to_list(node()), ".log"])),
  FullMessage = Message ++ ["\n"],
  util:logging(Logfile, lists:concat(FullMessage)),
  Logfile.

maybe_log_wrong_term(CmiReceived, MinimumCmi) when CmiReceived > MinimumCmi ->
  log(["Error! Received terminated ggT is bigger than the smalles received ggT (", integer_to_list(CmiReceived), ",", integer_to_list(MinimumCmi), ")"]);
maybe_log_wrong_term(_CmiReceived, _MinimumCmi) when true ->
  ok.

handle_briefterm(ActVal, CMi, ClientName, CZeit) ->
  Minimum = maps:get(smallestGgT, ActVal),
  if
    CMi > Minimum ->
      log(["Trying to correct with sending ", integer_to_list(Minimum), " to ", atom_to_list(ClientName), " at ", vsutil:now2string(CZeit)]),
      maps:get(ClientName, maps:get(clientsToPID, ActVal)) ! {sendy, Minimum};
    true ->
      log([atom_to_list(ClientName), " reports correct termination with ggT ", CMi, " at ", vsutil:now2string(CZeit)])
  end.

prompt_all_ggt(_ClientsToPID, []) -> ok;
prompt_all_ggt(ClientsToPID, [Client | RestClients]) ->
  maps:get(Client, ClientsToPID) ! {self(), tellmi},
  receive
    {mi, Mi} -> log(["client: ", Client, " has mi: ", Mi])
  end,
  prompt_all_ggt(ClientsToPID, RestClients).

check_status_all_ggt(_ClientsToPID, []) -> ok;
check_status_all_ggt(ClientsToPID, [Client | RestClients]) ->
  {ClientName, ClientNode} = maps:get(Client, ClientsToPID),
  PingResponse = net_adm:ping(ClientNode),
  if
    PingResponse =:= pang ->
      log(["client node of ", Client, " is dead"]);
    true ->
      {ClientName, ClientNode} ! {self(), pingGGT},
      receive
        {pongGGT, GgTName} -> log(["ggT-Process: ", GgTName, " is alive"])
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
  log(["Start calculation for ggT: ", integer_to_list(WggT)]),
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
      log(["Correct flag is now set to: ", NewFlag, " from: ", CorrectFlag]),
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
      log([atom_to_list(ClientName), " reports new Mi ", integer_to_list(CMi), " at ", vsutil:now2string(CZeit)]),
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
          log([atom_to_list(ClientName), " reports termination with ggT ", CMi, " at ", vsutil:now2string(CTermZeit)])
      end,
      calculation(ActVal)
  end.

set_neighbors(ClientsToPID, [Middle, Last], [First, Second | _Tail]) ->
  log(["Set neighbour for ggT-process ", atom_to_list(Last), " with neighbours: ", atom_to_list(Middle), " ", atom_to_list(First)]),
  maps:get(Last, ClientsToPID) ! {setneighbors, Middle, First},
  log(["Set neighbour for ggT-process ", atom_to_list(First), " with neighbours: ", atom_to_list(Last), " ", atom_to_list(Second)]),
  maps:get(First, ClientsToPID) ! {setneighbors, Last, Second};

set_neighbors(ClientsToPID, [Left, Middle, Right | Tail], Clients) ->
  log(["Set neighbour for ggT-process ", atom_to_list(Middle), " with neighbours: ", atom_to_list(Left), " ", atom_to_list(Right)]),
  maps:get(Middle, ClientsToPID) ! {setneighbors, Left, Right},
  set_neighbors(ClientsToPID, [Middle, Right] ++ Tail, Clients).

bind_ggT(NameService, GgTName, ClientsToPID) ->
  NameService ! {self(), {lookup, GgTName}},
  receive
    not_found ->
      log(["Could not bind ", atom_to_list(GgTName)]),
      maps:put(GgTName, undefined, ClientsToPID);
    {pin, GgTPID} ->
      log(["Bound ggT-process ", atom_to_list(GgTName)]),
      maps:put(GgTName, GgTPID, ClientsToPID)
  end.

bind_ggTs(ActVal) ->
  Config = maps:get(config, ActVal),
  {ok, NSNode} = vsutil:get_config_value(nameservicenode, Config),
  {ok, NSName} = vsutil:get_config_value(nameservicename, Config),
  pong = net_adm:ping(NSNode),
  NameService = global:whereis_name(NSName),
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
  log(["Shutting down ", integer_to_list(length(maps:get(clients, ActVal))), " ggT-processes"]),
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
      log(["getsteeringval: ", pid_to_list(From)]),
      initial(ActVal);
    {hello, ClientName} ->
      NewActVal = maps:update(clients, maps:get(clients, ActVal) ++ [ClientName], ActVal),
      log(["hello: ", atom_to_list(ClientName), " #ofclients: ", integer_to_list(length(maps:get(clients, NewActVal)))]),
      initial(NewActVal);
    reset -> initial(maps:update(clients, [], ActVal));
    kill -> shutdown(ActVal);
    step -> Config = maps:get(config, ActVal),
      {ok, ExpectedClients} = vsutil:get_config_value(ggtprozessnummer, Config),
      ActualClients = length(maps:get(clients, ActVal)),
      log(["Initial ActVal completed. Registered ", integer_to_list(ExpectedClients), "/", integer_to_list(ActualClients), " ggT-processes"]),
      log(["Start building ring"]),
      BoundClientsActVal = bind_ggTs(ActVal),
      % build ring
      ShuffledClients = util:shuffle(maps:get(clients, ActVal)),
      set_neighbors(maps:get(clientsToPID, BoundClientsActVal), ShuffledClients, ShuffledClients),
      log(["Done building ring"]),
      log(["Switching to calculation ActVal"]),
      calculation(BoundClientsActVal)
  end,
  ActVal.

init_coordinator() ->
  {ok, Config} = file:consult("./config/koordinator.cfg"),
  log(["starttime: ", util:timeMilliSecond(), " | mit PID ", pid_to_list(self())]),
  log(["koordinator.cfg geoeffnet..."]),
  {ok, CoordName} = vsutil:get_config_value(koordinatorname, Config),
  log(["koordinator.cfg gelesen..."]),
  {ok, NSNode} = vsutil:get_config_value(nameservicenode, Config),
  {ok, NSName} = vsutil:get_config_value(nameservicename, Config),
  pong = net_adm:ping(NSNode),
  NameService = global:whereis_name(NSName),
  log(["Nameservice ", pid_to_list(NameService), " gebunden..."]),
  erlang:register(CoordName, self()),
  log(["lokal registriert..."]),
  NameService ! {self(), {rebind, CoordName, node()}},
  receive
    ok -> log(["beim Namensdienst registriert."])
  end,
  initial(#{config => Config, clients => [], clientsToPID => #{}, smallestGgT => 134217728}).

start() ->
  spawn(?MODULE, init_coordinator, []).
