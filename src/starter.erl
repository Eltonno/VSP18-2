-module(starter).

-export([go/2, start/1, pingCoordinator/2, get_steering_values/2, ggT_id/4, start_ggT_processes/4]).

pingCoordinator(NameService, Config) ->
  {ok, CoordinatorName} = vsutil:get_config_value(koordinatorname, Config),
  NameService ! {self(), {lookup, CoordinatorName}},
  {ok, StarterID} = vsutil:get_config_value(starterid, Config),
  receive
    not_found ->
      util:logging(list_to_atom("ggt" ++ integer_to_list(StarterID) ++  "@" ++ atom_to_list(node()) ++ ".log"), "service " ++ atom_to_list(CoordinatorName) ++ " not found...\n"),
      {};
    {pin, {Name, Node}} ->
      pong = net_adm:ping(Node),
      util:logging(list_to_atom("ggt" ++ integer_to_list(StarterID) ++  "@" ++ atom_to_list(node()) ++ ".log"), "coordinator service " ++ atom_to_list(Name) ++ "(" ++ atom_to_list(Node) ++ ") bound...\n"),
      {Name, Node}
  end.

get_steering_values(Coordinator, Config) ->
  Coordinator ! {self(), getsteeringval},
  {ok, StarterID} = vsutil:get_config_value(starterid, Config),
  receive
    {steeringval, ArbeitsZeit, TermZeit, Quota, GGTProzessnummer} ->
      util:logging(list_to_atom("ggt" ++ integer_to_list(StarterID) ++  "@" ++ atom_to_list(node()) ++ ".log"), "getsteeringval: " ++ integer_to_list(ArbeitsZeit) ++ " work time; " ++ integer_to_list(TermZeit) ++ " term time; " ++ integer_to_list(Quota) ++ " quota; " ++ integer_to_list(GGTProzessnummer) ++ " #ggT proccesses."),
      [ArbeitsZeit, TermZeit, Quota, GGTProzessnummer]
  end.

ggT_id(GroupNumber, TeamNumber, GgTProcessNumber, StarterID) ->
  String = lists:concat([integer_to_list(GroupNumber), integer_to_list(TeamNumber), integer_to_list(GgTProcessNumber), integer_to_list(StarterID)]),
  list_to_atom(String).

-spec start_ggT_processes(integer(), map(), fun(), list()) -> any().
start_ggT_processes(0, _ParamMap, _Fun, Result) -> Result;
start_ggT_processes(NumberOfGgtProcesses, ParamMap, Fun, Result) ->
  GgTID = ggT_id(maps:get(groupnumber, ParamMap), maps:get(teamnumber, ParamMap), NumberOfGgtProcesses, maps:get(starterid, ParamMap)),
  NewResult = Result ++ [Fun(maps:get(worktime, ParamMap), maps:get(termtime, ParamMap), maps:get(quota, ParamMap), GgTID, maps:get(coordinator, ParamMap), maps:get(nameservice, ParamMap))],
  start_ggT_processes(NumberOfGgtProcesses - 1, ParamMap, Fun, NewResult).

%% Starts the starter with unique starterID
start(StarterID) ->
  {ok, Config} = file:consult("ggt.cfg"),
  NewConfig = lists:concat([Config, [{starterid, StarterID}]]),

  util:logging(list_to_atom("ggt" ++ integer_to_list(StarterID) ++  "@" ++ atom_to_list(node()) ++ ".log"), "Starttime: " ++ util:timeMilliSecond() ++ " with PID " ++ pid_to_list(self()) ++ "\n"),
  util:logging(list_to_atom("ggt" ++ integer_to_list(StarterID) ++  "@" ++ atom_to_list(node()) ++ ".log"), "ggt.cfg geöffnet...\n"),

  {ok, GroupNumber} = vsutil:get_config_value(praktikumsgruppe, Config),
  {ok, TeamNumber} = vsutil:get_config_value(teamnummer, Config),
  util:logging(list_to_atom("ggt" ++ integer_to_list(StarterID) ++  "@" ++ atom_to_list(node()) ++ ".log"), "ggt.cfg geladen...\n"),

  {ok, NSNode} = vsutil:get_config_value(nameservicenode, Config),
  pong = net_adm:ping(NSNode),
  NameService = global:whereis_name(nameservice),
  util:logging(list_to_atom("ggt" ++ integer_to_list(StarterID) ++  "@" ++ atom_to_list(node()) ++ ".log"), "Nameservice '" ++ pid_to_list(NameService) ++ "' bound...\n"),

  Coordinator = pingCoordinator(NameService, NewConfig),
  [ArbeitsZeit, TermZeit, Quota, NumberOfGgtProcesses] = get_steering_values(Coordinator, Config),
  ParamMap = #{worktime => ArbeitsZeit, termtime => TermZeit, starterid => StarterID, groupnumber => GroupNumber, teamnumber => TeamNumber, nameservice => NameService, coordinator => Coordinator, quota => Quota},
  start_ggT_processes(NumberOfGgtProcesses, ParamMap, fun ggt:start/6, []).

-spec go(integer(), integer()) -> any().
go(0, _) ->
  ok;
go(Anzahl, Start) ->
  spawn(?MODULE, start, [Start]),
  go(Anzahl - 1, Start +1).