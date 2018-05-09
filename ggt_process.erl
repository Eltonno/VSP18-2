-module(ggt_process).
-export([start/9, init_ggt/1, term_request/1]).

start(ArbeitsZeit,TermZeit,Quota,Praktikumsgruppe,Teamnummer,GGTProzessnummer,Starternummer,NameS,Koordinator) ->
  GGTName = list_to_atom(integer_to_list(Praktikumsgruppe) ++ integer_to_list(Teamnummer) ++ integer_to_list(GGTProzessnummer) ++ integer_to_list(Starternummer)),
  ActVal = 	[{ggtname, GGTName},
    {mi, undefined},
    {leftn, undefined},
    {rightn, undefined},
    {arbeitszeit, ArbeitsZeit},
    {termzeit, TermZeit},
    {quota, Quota},
    {names, NameS},
    {koordinator, Koordinator},
    {letztenachricht, vsutil:getUTC()},
    {tref, undefined},
    {votes, undefined}],
  spawn(?MODULE, init_ggt, [ActVal]).

init_ggt(ActVal) ->
  {ok, Config} = file:consult("ggt.cfg"),
  {ok, NameServiceNode} = vsutil:get_config_value(nameservicenode, Config),
  {ggtname, GGTName} = lists:keyfind(ggtname, 1, ActVal),
  {termzeit, TermZeit} = lists:keyfind(termzeit, 1, ActVal),
  util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ "\n"),
  {koordinator, Koordinator} = lists:keyfind(koordinator, 1, ActVal),
  Koordinator ! {hello, GGTName},
  util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", "ggt-" ++ util:to_String(GGTName) ++ "hat sich am Koordinator angemeldet\n"),
  net_adm:ping(NameServiceNode),
  NameService = global:whereis_name(nameservice),
  register(GGTName, self()),
  NameService ! {self(),{rebind,GGTName,node()}},
  receive
    ok -> util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", "ggt-" ++ util:to_String(GGTName) ++ "hat sich am Namensdienst angemeldet\n")
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
    {setneighbors,LeftN,RightN} ->
      LActVal = lists:keystore(leftn, 1, ActVal, {leftn, LeftN}),
      util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " RightN: " ++ atom_to_list(LActVal) ++ "\n"),
      RLActVal = lists:keystore(rightn, 1, LActVal, {rightn, RightN}),
      util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " RightN: " ++ atom_to_list(RLActVal) ++ "\n"),
      messages(RLActVal);
    {setpm, MiNeu} ->
      {ok, cancel} = timer:cancel(TRef),
      MActVal = lists:keystore(mi, 1, ActVal, {mi, MiNeu}),
      {ok, CTRef} = timer:apply_after(TermZeit,?MODULE,term_request, [MActVal]),
      TMActVal = lists:keystore(tref, 1, MActVal, {tref, CTRef}),
      LastTMActVal = lists:keystore(letztenachricht, 1, TMActVal, {letztenachricht, vsutil:getUTC()}),
      messages(LastTMActVal);
    {sendy, Y} ->
      util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", "setpm: " ++ integer_to_list(Y) ++ "\n"),
      util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " LeftN: " ++ atom_to_list(LeftN) ++ "\n"),
      util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " RightN: " ++ atom_to_list(RightN) ++ "\n"),
      if
        Y < Mi ->
          {ok, cancel} = timer:cancel(TRef),
          NewMi = ((Mi-1) rem Y) + 1,
          MActVal = lists:keystore(mi, 1, ActVal, {mi, NewMi}),
          NameService ! {self(),{lookup, LeftN}},
          receive
            not_found -> util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " LeftN not found\n");
            {pin, LeftNPID} -> LeftNPID ! {sendy, NewMi}
          end,
          NameService ! {self(),{lookup, RightN}},
          receive
            not_found -> util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ " RightN not found\n");
            {pin, RightNPID} -> RightNPID ! {sendy, NewMi}
          end,
          timer:sleep(ArbeitsZeit),
          Koordinator ! {briefmi,{GGTName,NewMi,util:timeMilliSecond()}},
          {ok, CTRef} = timer:apply_after(TermZeit,?MODULE,term_request, [MActVal]),
          TMActVal = lists:keystore(tref, 1, MActVal, {tref, CTRef}),
          LastTMActVal = lists:keystore(letztenachricht, 1, TMActVal, {letztenachricht, vsutil:getUTC()}),
          util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ "hat sein Mi upgedatet zu: " ++ integer_to_list(NewMi) ++ "\n"),
          messages(LastTMActVal);
        true ->
          util:logging("GGTP_" ++ atom_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", "Keine Änderung am Mi\n"),
          messages(ActVal)
      end;
    {From,{vote,Initiator}} ->
      CTime = vsutil:getUTC(),
      Comp = (LetzteNachricht + (TermZeit / 2)) - CTime,
      if
        Comp =< 0 ->
          From ! {voteYes, GGTName},
          messages(ActVal);
        true -> messages(ActVal)
      end;
    {voteYes,Name} ->
      VActVal = lists:keystore(votes, 1, ActVal, {votes, Votes+1}),
      if
        (Votes + 1) == Quota ->
          Koordinator ! {self(),briefterm,{GGTName,Mi,util:timeMilliSecond()}}
      end,
      messages(VActVal);
    {From,tellmi} ->
      From ! {mi, Mi},
      messages(ActVal);
    {From,pingGGT} ->
      From ! {pongGGT, GGTName},
      messages(ActVal);
    kill ->
      NameS ! {self(),{unbind, GGTName}},
      unregister(GGTName)
  end.

term_request(ActVal) ->
  {names, NameS} = lists:keyfind(names, 1, ActVal),
  {ggtname, GGTName} = lists:keyfind(ggtname, 1, ActVal),
  NameS ! {self(),{multicast, vote, GGTName}}.