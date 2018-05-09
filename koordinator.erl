-module(koordinator).
-export([start/0, initial/1]).

start() ->
  {ok, Config} = file:consult("koordinator.cfg"),
  {ok, ArbeitsZeit} = vsutil:get_config_value(arbeitszeit, Config),
  {ok, TermZeit} = vsutil:get_config_value(termzeit, Config),
  {ok, GGTProzessnummer} = vsutil:get_config_value(ggtprozessnummer, Config),
  {ok, NameServiceNode} = vsutil:get_config_value(nameservicenode, Config),
  {ok, Koordinatorname} = vsutil:get_config_value(koordinatorname, Config),
  {ok, Quote} = vsutil:get_config_value(quote, Config),
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
    {ggts, []},
    {nameservice, NameService},
    {minmi, undefined}],
  spawn(?MODULE, initial, [State]).

initial(State) ->
%%  util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "befor receive, while initial\n"),
  receive
    {From, getsteeringval} ->
      {arbeitszeit, ArbeitsZeit} = lists:keyfind(arbeitszeit, 1, State),
      {termzeit, TermZeit} = lists:keyfind(termzeit, 1, State),
      {quote, Quote} = lists:keyfind(quote, 1, State),
      {ggtprozessnummer, GGTProzessnummer} = lists:keyfind(ggtprozessnummer, 1, State),
      util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "util:to_String(NameService)\n"),
      From ! {steeringval,ArbeitsZeit,TermZeit,Quote,GGTProzessnummer},
      initial(State);
    {hello, Clientname} ->
      {ggts, GGTs} = lists:keyfind(ggts, 1, State),
      util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "util:to_String(NameService)\n"),
      NewState = lists:keystore(ggts, 1, State, {ggts, [GGTs|Clientname]}),
      initial(NewState);
    step ->
      {ggts, GGTs} = lists:keyfind(ggts, 1, State),
      util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "util:to_String(NameService)\n"),
      MixedGGTs = util:shuffle(GGTs),
      bereit(lists:keystore(ggts, 1, State, {ggts, MixedGGTs}))
  end.

bereit(State) ->
  {ggts, GGTs} = lists:keyfind(ggts, 1, State),
  {korrigieren, Korrigieren} = lists:keyfind(korrigieren, 1, State),
  {nameservice, NameService} = lists:keyfind(nameservice, 1, State),
  {koordinatorname, Koordinatorname} = lists:keyfind(koordinatorname, 1, State),
  {minmi, MinMi} = lists:keyfind(minmi, 1, State),
  ringbildung(GGTs, State, []),
  receive
    reset ->
      beenden(GGTs, State),
      initial(lists:keystore(ggts, 1, State, {ggts, []}));
    prompt ->
      prompt_ggts(GGTs, State),
      bereit(State);
    {briefmi,{Clientname,CMi,CZeit}} ->
      util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", atom_to_list(Clientname) ++ " liefert aktuelles Mi: " ++ integer_to_list(CMi) ++ " um " ++ util:to_String(CZeit) ++ "\n"),
      if
        CMi < MinMi -> NewState = lists:keystore(mi, 1, State, {mi, CMi}), bereit(NewState)
      end,
      bereit(State);
    {From, briefterm, {Clientname,CMi,CZeit}} ->
      if
        CMi > MinMi ->
          if
            Korrigieren == 1 ->
              From ! {sendy, MinMi},
              bereit(State);
            true ->
              util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", atom_to_list(Clientname) ++ " liefert fehlerhaftes Mi: " ++ integer_to_list(CMi) ++ " um " ++ util:to_String(CZeit) ++ " es ist bereits ein kleineres Mi bekannt: " ++ integer_to_list(MinMi) ++ "\n")
          end,
          NewState = lists:keystore(mi, 1, State, {mi, CMi}), bereit(NewState)
      end;
    nudge ->
      lists:foreach(fun(G) ->
        Pong = net_adm:ping(G),
        util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "Lebenszustand von " ++ atom_to_list(G) ++ " ist " ++ atom_to_list(Pong) ++ "\n")
                    end, GGTs),
      bereit(State);
    toggle ->
      bereit(lists:keystore(korrigieren, 1, State, {korrigieren, ((Korrigieren+1) rem 2)}));
    {calc, WggT} ->
      NoClients = max(2,trunc(length(GGTs)/5)),
      TwntyPrc = lists:nthtail((length(GGTs)-NoClients), util:shuffle(GGTs)),
      Mis = vsutil:bestimme_mis(WggT, (length(GGTs))),
      lists:foreach(fun(G)->
        NameService ! {self(),{lookup, list_to_atom(G)}},
        CMi = lists:nth(index_of(G, GGTs),Mis),
        receive
          not_found -> util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "GGT ist nicht auf derm Namensservice vorhanden\n");
          {pin, GGTNameNode} -> GGTNameNode ! {setpm, CMi}
        end
                    end, GGTs),
      Ys = vsutil:bestimme_mis(WggT, (length(TwntyPrc))),
      lists:foreach(fun(G)->
        NameService ! {self(),{lookup, list_to_atom(G)}},
        Y = lists:nth(index_of(G, TwntyPrc),Ys),
        receive
          not_found -> util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "GGT ist nicht auf derm Namensservice vorhanden\n");
          {pin, GGTNameNode} -> GGTNameNode ! {sendy, Y}
        end
                    end, TwntyPrc),
      bereit(State);
    kill ->
      beenden(GGTs, State),
      NameService ! {self(),{unbind, Koordinatorname}},
      erlang:exit(self(),normal)
  end.

ringbildung([GGT | Tail], State, GGTf) ->
  {nameservice, NameService} = lists:keyfind(nameservice, 1, State),
  NameService ! {self(),{lookup, list_to_atom(GGT)}},
  receive
    not_found -> util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", "GGT ist nicht auf derm Namensservice vorhanden\n");
    {pin, GGTNameNode} ->
      GGTNameNode ! {setneighbors, lists:last(Tail), lists:nth(1, Tail)},
      receive
        {mi, Mi} ->
          util:logging("Koordinator@" ++ atom_to_list(node()) ++ ".log", atom_to_list(GGT) ++ " hat aktuellen Mi: " ++ integer_to_list(Mi) ++ "\n"),
          if
            erlang:length(GGTf) /= erlang:length(Tail) ->
              ringbildung([Tail|GGT], State, [GGTf | GGT])
          end
      end
  end.

prompt_ggts([], _) -> ok;
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