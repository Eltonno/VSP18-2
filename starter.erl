-module(starter).
-export([start/1]).

%Startet den Starter mit einer eindeutigen ID.
start(StarterID) ->
  {ok, Hostname} = inet:gethostname(),
  Logfile = "ggt" ++ integer_to_list(StarterID) ++ "@" ++ Hostname ++ ".log",
  %Die restlichen Einstellungen werden aus der ggt.cfg-Datei ausgelesen.
  util:logging(Logfile , "Starter gestartet")
  {ok, Config} = file:consult("ggt.cfg"),
  {ok, Koordinatorname} = vsutil:get_config_value(koordinatorname, Config),
  {ok, NameServiceNode} = vsutil:get_config_value(nameservicenode, Config),
  {ok, Praktikumsgruppe} = vsutil:get_config_value(praktikumsgruppe, Config),
  {ok, Teamnummer} = vsutil:get_config_value(teamnummer, Config),

  pong = net_adm:ping(NameServiceNode),
  timer:sleep(1000),
%%KoordinatorNameNode = global:whereis_name(Koordinatorname),

  NameService = global:whereis_name(nameservice),
  NameService ! {self(),{lookup, Koordinatorname}},
  receive
    not_found -> util:logging(Logfile , "Koordinator nicht auf Nameservice vorhanden"),
      KoordinatorNameNode = {errname, errnode};
    {pin, {Name, Node}} -> 	pong = net_adm:ping(Node),
      KoordinatorNameNode = {Name, Node},
      {Name, Node} ! log
  end,
  util:logging(Logfile, util:to_String(KoordinatorNameNode) ++ "\n"),
  PID = self(),
  util:logging(Logfile, util:to_String(PID) ++ "\n"),
  % Der Starter bekommt einige Einstellungen vom Koordinator durch “getsteeringval”.
  KoordinatorNameNode ! {PID, getsteeringval},
  receive
    %Empfängt die Ein- stellungen und startet daraufhin die ggt-Prozesse via eigener ggt-Methode “ggt-starter”.
    {steeringval,ArbeitsZeit,TermZeit,Quote,GGTProzessnummer} ->
      util:logging(Logfile , "Hat steeringval bekommen"),
      State = [{arbeitszeit, ArbeitsZeit},
        {termzeit, TermZeit},
        {ggtprozessnummer, GGTProzessnummer},
        {koordinatorname, Koordinatorname},
        {quote, Quote},
        {nameservice, NameService},
        {praktikumsgruppe, Praktikumsgruppe},
        {starterid, StarterID},
        {teamnummer, Teamnummer},
        {logfile, Logfile}],
      ggt_starter(1,State),
      exit(self(), normal);
    not_found -> util:logging(Logfile, "Steeringval nicht gefunden")
  %%ggt_process:start(ArbeitsZeit,TermZeit,trunc(Quote*0,8),Teamnummer,GGTProzessnummer,StarterID,NameService,KoordinatorNameNode)
  end.

ggt_starter(Iterator, State) ->
  {arbeitszeit, ArbeitsZeit} = lists:keyfind(arbeitszeit, 1, State),
  {termzeit, TermZeit} = lists:keyfind(termzeit, 1, State),
  {ggtprozessnummer, GGTProzessnummer} = lists:keyfind(ggtprozessnummer, 1, State),
  {koordinatorname, KoordinatorName} = lists:keyfind(koordinatorname, 1, State),
  {quote, Quote} = lists:keyfind(quote, 1, State),
  {nameservice, NameService} = lists:keyfind(nameservice, 1, State),
  {praktikumsgruppe, Praktikumsgruppe} = lists:keyfind(praktikumsgruppe, 1, State),
  {teamnummer, Teamnummer} = lists:keyfind(teamnummer, 1, State),
  {starterid, StarterID} = lists:keyfind(starterid, 1, State),
  spawn(ggt_process, start, [ArbeitsZeit*1000,TermZeit*1000,Quote,Praktikumsgruppe,Teamnummer,Iterator,StarterID,NameService,KoordinatorName]),
  if
    Iterator == GGTProzessnummer ->
      ok;
    true ->
      ggt_starter(Iterator+1,State)
  end.