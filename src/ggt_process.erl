-module(ggt_process).
-export([start/9, init_ggt/1]).

start(ArbeitsZeit,TermZeit,Quota,Praktikumsgruppe,Teamnummer,GGTProzessnummer,Starternummer,NameS,Koordinator) ->
GGTName = list_to_atom(integer_to_list(Praktikumsgruppe) ++ integer_to_list(Teamnummer) ++ integer_to_list(GGTProzessnummer) ++ integer_to_list(Starternummer)),
ActVal = [{ggtname, GGTName},{arbeitszeit, ArbeitsZeit},{termzeit, TermZeit},{quota, Quota},{names, NameS},{koordinator, Koordinator}],
spawn(?MODULE, start_ggt_prozess, [ActVal]).

init_ggt(ActVal) ->
{ok, Config} = file:consult("ggt.cfg"),
{ok, NameServiceNode} = vsutil:get_config_value(nameservicenode, Config),
%%{names, NameS} = lists:keyfind(names, 1, ActVal),
{ggtname, GGTName} = lists:keyfind(ggtname, 1, ActVal),
util:logging("GGTP_" ++ integer_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", util:to_String(GGTName) ++ "\n"),
{koordinator, Koordinator} = lists:keyfind(koordinator, 1, ActVal),
Koordinator ! {hello, GGTName},
util:logging("GGTP_" ++ integer_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", "ggt-" ++ util:to_String(GGTName) ++ "hat sich am Koordinator angemeldet\n"),
%%{names, NameS} = lists:keyfind(names, 1, ActVal),
net_adm:ping(NameServiceNode),
NameService = global:whereis_name(nameservice),
register(GGTName, self()),
NameService ! {self(),{rebind,GGTName,node()}},
recieve
	ok -> util:logging("GGTP_" ++ integer_to_list(GGTName) ++ "@" ++ atom_to_list(node()) ++ ".log", "ggt-" ++ util:to_String(GGTName) ++ "hat sich am Namensdienst angemeldet\n")
end,
messages(ActVal).

messages(ActVal) ->
recieve
	{setneighbors,LeftN,RightN} ->
		lists:keystore(leftn, 1, ActVal, {leftn, LeftN}),
		lists:keystore(rightn, 1, ActVal, {rightn, RightN}}),
		messages(ActVal);
	{setpm, MiNeu} ->
		lists:keystore(mi, 1, ActVal, {mi, MiNeu}),
		messages(ActVal);
	{sendy, Y} ->
end.