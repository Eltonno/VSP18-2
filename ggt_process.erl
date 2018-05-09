-module(ggt_process).
-export([start/9, init_ggt/1, term_request/1]).

start(ArbeitsZeit,TermZeit,Quota,Praktikumsgruppe,Teamnummer,GGTProzessnummer,Starternummer,NameS,Koordinator) ->

  %Der Name des ggt-Prozesses ist eine Zahlenfolge die folgendermaßen erstellt wird: <PraktikumsgruppenID><TeamID><Nummer des ggT-Prozess><Nummer des Starters>
  GGTName = list_to_atom(integer_to_list(Praktikumsgruppe) ++ integer_to_list(Teamnummer) ++ integer_to_list(GGTProzessnummer) ++ integer_to_list(Starternummer)),

  %Der ggt-Prozess hat eine Liste von Tupeln ‘ActVal’, die folgende Daten speichert: – Einstellungen aus der Config.
  %– Aktuelle Mi-Zahl
  %– Timestamp der letzten Änderung von Mi
  %– Die Nachbarn
  %– Anzahl der Empfangenen yesVotes.

ActVal = 	[{ggtname, GGTName},
    {mi, undefined},   %– Aktuelle Mi-Zahl
  {leftn, undefined},   %– Die Nachbarn
  {rightn, undefined},   %– Die Nachbarn
  {arbeitszeit, ArbeitsZeit},
    {termzeit, TermZeit},
    {quota, Quota},
    {names, NameS},
    {koordinator, Koordinator},
    {letztenachricht, vsutil:getUTC()},   %– Timestamp der letzten Änderung von Mi
  {tref, undefined},
    {votes, undefined}],   %– Anzahl der Empfangenen yesVotes.
  spawn(?MODULE, init_ggt, [ActVal]).

init_ggt(ActVal) ->
  {ok, Config} = file:consult("ggt.cfg"),
  {ok, NameServiceNode} = vsutil:get_config_value(nameservicenode, Config),
  {ggtname, GGTName} = lists:keyfind(ggtname, 1, ActVal),
  {termzeit, TermZeit} = lists:keyfind(termzeit, 1, ActVal),
  util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ "\n"),
  {koordinator, Koordinator} = lists:keyfind(koordinator, 1, ActVal),
  %Der ggT-Prozess meldet sich beim Koordinator mit seinem Namen an (hello)
  Koordinator ! {hello, GGTName},
  util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " hat sich am Koordinator angemeldet\n"),
  net_adm:ping(NameServiceNode),
  NameService = global:whereis_name(nameservice),
  %Er registriert sich ebenfalls lokal auf der Erlang-Node mit seinem Namen (register)
  register(GGTName, self()),
  %und beim Namensdienst (rebind)
  NameService ! {self(),{rebind,GGTName,node()}},
  receive
    ok -> util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " hat sich am Namensdienst angemeldet\n")
  end,
  {ok, TRef} = timer:apply_after(TermZeit,?MODULE,term_request,[ActVal]),
  messages(lists:keystore(nameservice, 1,lists:keystore(tref, 1, ActVal, {tref, TRef}),{nameservice, NameService})).

messages(ActVal) ->
  {mi, Mi} = lists:keyfind(mi, 1, ActVal),
  {koordinator, Koordinator} = lists:keyfind(koordinator, 1, ActVal),
  {ggtname, GGTName} = lists:keyfind(ggtname, 1, ActVal),
  {arbeitszeit, ArbeitsZeit} = lists:keyfind(arbeitszeit, 1, ActVal),
  {leftn, LeftN} = lists:keyfind(leftn, 1, ActVal),
  {rightn, RightN} = lists:keyfind(rightn, 1, ActVal),
  {termzeit, TermZeit} = lists:keyfind(termzeit, 1, ActVal),
  {tref, TRef} = lists:keyfind(tref, 1, ActVal),
  {names, NameS} = lists:keyfind(names, 1, ActVal),
  {votes, Votes} = lists:keyfind(votes, 1, ActVal),
  {quota, Quota} = lists:keyfind(quota, 1, ActVal),
  {letztenachricht, LetzteNachricht} = lists:keyfind(letztenachricht, 1, ActVal),
  {nameservice, NameService} = lists:keyfind(nameservice, 1, ActVal),
  receive
    % Der ggT-Prozess erwartet dann vom Koordinator die Informationen über seine Nachbarn (setneighbors).
    {setneighbors,LeftN,RightN} ->

      util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " setneighbors -> LeftN: " ++ atom_to_list(LeftN) ++ "\n"),

      %Setzt die Nachbarn in Liste ‘ActVal’
      LActVal = lists:keystore(leftn, 1, ActVal, {leftn, LeftN}),
      util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " RightN: " ++ atom_to_list(LActVal) ++ "\n"),
      RLActVal = lists:keystore(rightn, 1, LActVal, {rightn, RightN}),
      util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " RightN: " ++ atom_to_list(RLActVal) ++ "\n"),
      messages(RLActVal);
    %Vor einer ggT-Berechnung erwartet der ggT-Prozess vom Koordinator seine Zahl Mi (setpm).
    {setpm, MiNeu} ->
      {ok, cancel} = timer:cancel(TRef),
      %Mi wird in ‘ActVal’ aktualisiert.
      MActVal = lists:keystore(mi, 1, ActVal, {mi, MiNeu}),
      {ok, CTRef} = timer:apply_after(TermZeit,?MODULE,term_request, [MActVal]),
      TMActVal = lists:keystore(tref, 1, MActVal, {tref, CTRef}),
      LastTMActVal = lists:keystore(letztenachricht, 1, TMActVal, {letztenachricht, vsutil:getUTC()}),
      messages(LastTMActVal);
    {sendy, Y} ->
      util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " " ++ integer_to_list(Y) ++ "\n"),
      util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " LeftN: " ++ atom_to_list(LeftN) ++ "\n"),
      util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " RightN: " ++ atom_to_list(RightN) ++ "\n"),
      if
        %Euklid-Berechnung
      %Der rekursive Aufruf der ggT Berechnung.
      %Er startet nur die Berechnung, falls Y < Mi ist.
        Y < Mi ->
          util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", "Macht Euklid-Berechnung " ++ "\n"),

          {ok, cancel} = timer:cancel(TRef),
          NewMi = ((Mi-1) rem Y) + 1,
          MActVal = lists:keystore(mi, 1, ActVal, {mi, NewMi}),
          {nameservice, list_to_atom("auskunft@Mato.local")} ! {self(),{lookup, LeftN}},
          receive
            not_found -> util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " LeftN not found\n");
            {pin, LeftNPID} -> LeftNPID ! {sendy, NewMi}
          end,
          {nameservice, list_to_atom("auskunft@Mato.local")} ! {self(),{lookup, RightN}},
          receive
            not_found -> util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " RightN not found\n");
            {pin, RightNPID} -> RightNPID ! {sendy, NewMi}
          end,
          %Bei jeder Berechnung tut der ggt für die Dauer der Arbeitszeit nichts.
          timer:sleep(ArbeitsZeit),
          %Wenn sich die Zahl des ggt durch eine Berechnung geändert hat, teilt er dies dem Koordinator mit (briefmi).
          Koordinator ! {briefmi,{GGTName,NewMi,util:timeMilliSecond()}},
          %Wenn der ggt seit <Termzeit> keine Zahl empfangen hat, sendet er eine Terminierungsanfrage.
          {ok, CTRef} = timer:apply_after(TermZeit,?MODULE,term_request, [MActVal]),
          TMActVal = lists:keystore(tref, 1, MActVal, {tref, CTRef}),
          LastTMActVal = lists:keystore(letztenachricht, 1, TMActVal, {letztenachricht, vsutil:getUTC()}),
          util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log",util:to_String(GGTName) ++ " hat sein Mi upgedatet zu: " ++ integer_to_list(NewMi) ++ "\n"),
          messages(LastTMActVal);
        true ->
          util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " Keine Änderung am Mi\n"),
          messages(ActVal)
      end;
    %Wenn der ggt eine Terminierungsanfrage empfängt, so antwortet er mit yes, falls er seit seiner halben Termzeit keine Zahl empfangen hat.
  % Ansonsten ignoriert er diese Terminierungsanfrage.
    {From,{vote,Initiator}} ->
      CTime = vsutil:getUTC(),
      Comp = (LetzteNachricht + (TermZeit / 2)) - CTime,
      if
        Comp =< 0 ->
          util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", "Stimmt YES " ++ "\n"),

          From ! {voteYes, GGTName},
          messages(ActVal);

        true ->
          util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", "Stimmt nicht ab " ++ "\n"),

          messages(ActVal)

end;
    {voteYes,Name} ->
      VActVal = lists:keystore(votes, 1, ActVal, {votes, Votes+1}),
      %Wenn diese angenommen wurde, weil die Quote erreicht wurde, sendet er dem Koordinator eine Mitteilung über die Terminierung der aktuellen Berechnung,
      %die seinen Namen, den errechneten ggT (sein aktuelles Mi) und seine aktuelle Systemzeit beinhaltet.
      if
        (Votes + 1) == Quota ->
          util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", "Quota wurde erreicht " ++ "\n"),

          Koordinator ! {self(),briefterm,{GGTName,Mi,util:timeMilliSecond()}}
      end,
      messages(VActVal);
    {From,tellmi} ->
      %Sendet das aktuelle Mi (aus ‘state’) an From (ist PID)
      From ! {mi, Mi},
      messages(ActVal);
    {From,pingGGT} ->
      %Sendet ein pongGGT an From (ist PID)
      From ! {pongGGT, GGTName},
      messages(ActVal);
    kill ->
      %Loggt, dass der GGT gekillt wird.
      util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", "Wird gekilled " ++ "\n"),

      %Unbindet vom Namensdienst via {From, {unbind, GGTName}}
      NameS ! {self(),{unbind, GGTName}},
      %Terminiert sich selbst.
      unregister(GGTName)
  end.

term_request(ActVal) ->
  {names, NameS} = lists:keyfind(names, 1, ActVal),
  {ggtname, GGTName} = lists:keyfind(ggtname, 1, ActVal),
  NameS ! {self(),{multicast, vote, GGTName}}.