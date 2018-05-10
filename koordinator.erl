-module(koordinator).
-export([start/0, initial/1]).

start() ->
  %Die Einstellungen für den Koordinator werden aus der koordinator.cfg-Datei ausgelesen.
  %Diese beinhaltet:
  {ok, Config} = file:consult("koordinator.cfg"),
  %Verzögerungszeit für die Berechnung (arbeitszeit)
  {ok, ArbeitsZeit} = vsutil:get_config_value(arbeitszeit, Config),
  %Lebenszeit (termzeit)
  {ok, TermZeit} = vsutil:get_config_value(termzeit, Config),
  %Anzahl der zu startenden GGT-Prozesse (ggtprozessnummer)
  {ok, GGTProzessnummer} = vsutil:get_config_value(ggtprozessnummer, Config),
  %Namensdienst Node (nameservicenode)
  {ok, NameServiceNode} = vsutil:get_config_value(nameservicenode, Config),
  %Name des Koordinators (koordinatorname)
  {ok, Koordinatorname} = vsutil:get_config_value(koordinatorname, Config),
  %Für die Abstimmung benötigte Quote in % (quote)
  {ok, Quote} = vsutil:get_config_value(quote, Config),
  %Ob der Koordinator bei Terminierungsmeldungen korrigierend eingreifen soll (korrigieren)
  {ok, Korrigieren} = vsutil:get_config_value(korrigieren, Config),
  register(Koordinatorname, self()),
  pong = net_adm:ping(NameServiceNode),
  timer:sleep(1000),
  %%util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", util:to_String(Pong)),
  NameService = global:whereis_name(nameservice),
  util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", util:to_String(NameService)),
  NameService ! {self() ,{rebind, Koordinatorname, node()}},
  receive
    ok -> ok
  end,

  State = [{arbeitszeit, ArbeitsZeit},
    {termzeit, TermZeit},
    {ggtprozessnummer, GGTProzessnummer},
    {nameservicenode, NameServiceNode},
    {koordinatorname, Koordinatorname},
    {quote, Quote},
    {korrigieren, Korrigieren},
    %Der Koordinator hat eine Liste der bei ihm registrierten ggt-Prozesse,
    {ggts, []},
    {nameservice, NameService},
    %und eine Variable, in der die kleinste von den ggt-Prozessen empfangene Zahl gespeichert wird.
    {minmi, undefined}],
  %Anfangs ist der Koordinator im Zustand “initial”.
  spawn(?MODULE, initial, [State]).

%initial
initial(State) ->
%%  util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "befor receive, while initial\n"),
  receive
    %Hier sendet er dem Starter, wenn von dem Starter angefragt,
  % die ggtprozessnummer, arbeitszeit, termzeit, und ggt- prozessnummer * 0.<quote>
    {From, getsteeringval} ->
      {arbeitszeit, ArbeitsZeit} = lists:keyfind(arbeitszeit, 1, State),
      {termzeit, TermZeit} = lists:keyfind(termzeit, 1, State),
      {quote, Quote} = lists:keyfind(quote, 1, State),
      {ggtprozessnummer, GGTProzessnummer} = lists:keyfind(ggtprozessnummer, 1, State),
      util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "util:to_String(NameService)\n"),
      %Sendet From (Starter) seine Einstellungs-Werte.
      %Dies sind die Arbeitszeit, die Termzeit, die Quote, und die GGTProzessnummer.
      From ! {steeringval,ArbeitsZeit,TermZeit,Quote,GGTProzessnummer},
      initial(State);
  %Registriert den ggt-Prozess.
    {hello, Clientname} ->
      {ggts, GGTs} = lists:keyfind(ggts, 1, State),
      util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "util:to_String(NameService)\n"),
      %Dazu fügt der Koordinator den ggt- Prozess in seine ggt-Prozess-Liste ein.
      NewState = lists:keystore(ggts, 1, State, {ggts, [GGTs|Clientname]}),
      initial(NewState);
    step ->
      %• Empfängt der Koordinator den Befehl “step” vom Benutzer,
      % so beendet er den “initial”-Zustand in dem er die ggT-Prozesse per Zufall in einem Ring anordnet.
      {ggts, GGTs} = lists:keyfind(ggts, 1, State),
      util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "util:to_String(NameService)\n"),
      %Die Liste der registrierten ggt-Prozesse mischen
      MixedGGTs = util:shuffle(GGTs),
      ringbildung(GGTs, State, []),
      %Daraufhin ist er im Zustand “bereit”.
      bereit(lists:keystore(ggts, 1, State, {ggts, MixedGGTs}))
  end.

%bereit
bereit(State) ->
  {ggts, GGTs} = lists:keyfind(ggts, 1, State),
  {korrigieren, Korrigieren} = lists:keyfind(korrigieren, 1, State),
  {nameservice, NameService} = lists:keyfind(nameservice, 1, State),
  {koordinatorname, Koordinatorname} = lists:keyfind(koordinatorname, 1, State),
  {minmi, MinMi} = lists:keyfind(minmi, 1, State),
  receive
    reset ->
      %Beendet alle ggt-Prozesse (kill), leert die ggt-Prozessliste und setzt den Zustand auf initial.
      beenden(GGTs, State),
      initial(lists:keystore(ggts, 1, State, {ggts, []}));
    prompt ->
      prompt_ggts(GGTs, State),
      bereit(State);
    %Empfängt die neue Mi des ggt und einen Timestamp.
    {briefmi,{Clientname,CMi,CZeit}} ->
      %Loggt dies.
      util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", atom_to_list(Clientname) ++ " liefert aktuelles Mi: " ++ integer_to_list(CMi) ++ " um " ++ util:to_String(CZeit) ++ "\n"),
      if
        %Falls diese Mi die bisher kleinste empfangene Mi ist, wird dies dementsprechend gespeichert.
        CMi < MinMi -> NewState = lists:keystore(mi, 1, State, {mi, CMi}), bereit(NewState)
      end,
      bereit(State);
    %Empfängt vom ggt das Ergebnis der beendeten Berechnung, sowie einen Timestamp.
    {From, briefterm, {Clientname,CMi,CZeit}} ->
      if
        %Dann überprüft der Koordinator ob die empfangene Mi größer als die gespeicherte kleinste Mi ist.
        CMi > MinMi ->
          if
            %Falls dies so ist und ‘korrigiere” 1 ist, teilt er dem ggt diese kleinste Zahl per “sendy” mit.
            Korrigieren == 1 ->
              From ! {sendy, MinMi},
              bereit(State);
            true ->
              %Falls dies so ist und “korrigiere” 0 ist, schreibt er dies nur als Fehlermeldung in die Log-Datei.
              util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", atom_to_list(Clientname) ++ " liefert fehlerhaftes Mi: " ++ integer_to_list(CMi) ++ " um " ++ util:to_String(CZeit) ++ " es ist bereits ein kleineres Mi bekannt: " ++ integer_to_list(MinMi) ++ "\n")
          end,
          %Ansonsten wird diese Mi als kleinste empfangene Mi gespeichert.
          NewState = lists:keystore(mi, 1, State, {mi, CMi}), bereit(NewState)
      end;
    nudge ->
      lists:foreach(fun(G) ->
        %Iteriert durch die ggt-Liste und erfragt bei jedem ggt seinen aktuelle Lebenszustand (per “pingGGT”) und loggt diesen.
        Pong = net_adm:ping(G),
        util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "Lebenszustand von " ++ atom_to_list(G) ++ " ist " ++ atom_to_list(Pong) ++ "\n")
                    end, GGTs),
      bereit(State);
    %Wechselt den Wert von “korrigiere” entweder von 0 auf 1 oder von 1 auf 0.
    toggle ->
      % Dies wird durch die Formel (“korrigiere” + 1) mod 2 getan
      bereit(lists:keystore(korrigieren, 1, State, {korrigieren, ((Korrigieren+1) rem 2)}));
    {calc, WggT} ->
      %Empfängt der Koordinator den Befehl “calc”, so informiert er die ggt-Prozesse über deren Mi-Wert
      %und startet bei 20% der Prozesse die Berechnung.
      NoClients = max(2,trunc(length(GGTs)/5)),
      %Wählt 20% der ggt-Prozesse aus der ggt-Liste aus
      TwntyPrc = lists:nthtail((length(GGTs)-NoClients), util:shuffle(GGTs)),
      Mis = vsutil:bestimme_mis(WggT, (length(GGTs))),
      lists:foreach(fun(G)->
        NameService ! {self(),{lookup, list_to_atom(G)}},
        CMi = lists:nth(index_of(G, GGTs),Mis),
        receive
          not_found -> util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "GGT ist nicht auf derm Namensservice vorhanden\n");
          %Sendet diesen ggt-Prozessen eine Zahl
          {pin, GGTNameNode} -> GGTNameNode ! {setpm, CMi}
        end
                    end, GGTs),
      Ys = vsutil:bestimme_mis(WggT, (length(TwntyPrc))),
      lists:foreach(fun(G)->
        NameService ! {self(),{lookup, list_to_atom(G)}},
        Y = lists:nth(index_of(G, TwntyPrc),Ys),
        receive
          not_found -> util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "GGT ist nicht auf derm Namensservice vorhanden\n");
          %und startet mit sendy rekursiv die Berechnung.
          {pin, GGTNameNode} -> GGTNameNode ! {sendy, Y}
        end
                    end, TwntyPrc),
      bereit(State);
    %Der Koordinator kann durch den manuellen Befehl “kill” terminiert werden
    kill ->
      %Hier wechselt er dann in den Zustand “beenden”, wo er zuerst alle ggt-Prozesse beendet und dann sich selbst.
      beenden(GGTs, State),
      NameService ! {self(),{unbind, Koordinatorname}},
      %Beendet sich selbst
      erlang:exit(self(),normal)
  end.

ringbildung([GGT | Tail], State, GGTf) ->
  {nameservice, NameService} = lists:keyfind(nameservice, 1, State),
  NameService ! {self(),{lookup, list_to_atom(GGT)}},
  receive
    not_found -> util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "GGT ist nicht auf derm Namensservice vorhanden\n");
    {pin, GGTNameNode} ->
      %Immer den ersten und letzten ggt_prozess durch setneighbors als Nachbarn setzen,
      GGTNameNode ! {setneighbors, lists:last(Tail), lists:nth(1, Tail)},
      receive
        {mi, Mi} ->
          util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", atom_to_list(GGT) ++ " hat aktuellen Mi: " ++ integer_to_list(Mi) ++ "\n"),
          if
            %Den ersten GGT der Liste ans Ende der Liste packen, bis dies mit allen einmal gemacht wurde.
          %Dies wird festgestellt, da alle GGTs die schon behandelt wurden in eine zweite Liste kommen, und wenn die gleichlang ist wie GGTs, wurden alle behandelt.
            erlang:length(GGTf) /= erlang:length(Tail) ->
              ringbildung([Tail|GGT], State, [GGTf | GGT])
          end
      end
  end.

prompt_ggts([], _) -> ok;
%Iteriert durch die ggt-Liste und erfragt bei jedem ggt seine aktuelle Mi (per “tellmi”) und loggt diese.
prompt_ggts([ GGT | Tail ], State) ->
  {nameservice, NameService} = lists:keyfind(nameservice, 1, State),
  NameService ! {self(),{lookup, list_to_atom(GGT)}},
  receive
    not_found -> util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "GGT ist nicht auf derm Namensservice vorhanden\n");
    {pin, GGTNameNode} ->
      GGTNameNode ! {self(), tellmi},
      receive
        {mi, Mi} ->
          util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", atom_to_list(GGT) ++ " hat aktuellen Mi: " ++ integer_to_list(Mi) ++ "\n"),
          prompt_ggts(Tail, State)
      end
  end.

%beenden
beenden([], _) -> ok;
beenden([ GGT | Tail ], State) ->
%%	
%%	foreach(fun, List),
%%
  {nameservice, NameService} = lists:keyfind(nameservice, 1, State),
  NameService ! {self(),{lookup, list_to_atom(GGT)}},
  receive
    not_found -> util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "GGT ist nicht auf derm Namensservice vorhanden\n");
    {pin, GGTNameNode} -> GGTNameNode ! kill, beenden(Tail, NameService)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).