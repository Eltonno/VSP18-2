-module(vsutil).
-export([get_config_value/2,
         openSe/2,openSeA/2,openRec/3,openRecA/3,createBinaryS/1,createBinaryD/1,createBinaryT/1,createBinaryNS/1,concatBinary/4,concatBinary/3,message_to_string/1,
		 reset_timer/3,compareNow/2,getUTC/0,compareUTC/2,now2UTC/1,now2string/1,now2stringD/1,
		 validTS/1,lessTS/2,lessoeqTS/2,equalTS/2,diffTS/2,
		 bestimme_mis/2,testeMI/2]).
-import(util, [concat/2,klebe/2,toMilliSeconds/1]).
		 
-define(TAUS, 1000).
-define(MILL, 1000000).

-define(TTL, 1).

%% -------------------------------------------
% Werkzeug
%% -------------------------------------------
%%
%% -------------------------------------------
%%
% Sucht aus einer Config-Liste die gewÃ¼nschten EintrÃ¤ge
% Beispielaufruf: 	{ok, ConfigListe} = file:consult("server.cfg"),
%                  	{ok, Lifetime} = get_config_value(lifetime, ConfigListe),
%
get_config_value(Key, []) ->
	{nok, Key};
get_config_value(Key, [{Key, Value} | _ConfigT]) ->
	{ok, Value};
get_config_value(Key, [{_OKey, _Value} | ConfigT]) ->
	get_config_value(Key, ConfigT).

%% -------------------------------------------
%%
% Unterbricht den aktuellen Timer
% und erstellt einen neuen und gibt ihn zurÃ¼ck
%%
reset_timer(Timer,Sekunden,Message) ->
	case timer:cancel(Timer) of
		{error, _Reason} ->
				neu;
		{ok, cancel} ->
				alt
 	end,
	{ok,TimerNeu} = timer:send_after(Sekunden*1000,Message),
	TimerNeu.
	
%% -------------------------------------------
%%
%% UTC Zeitstempel
getUTC() ->
	{MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
	% MicroSecs ist eine Zahl der Form ***000, deshalb div 1000
	((((MegaSecs * ?MILL) + Secs) * ?TAUS) + (MicroSecs div ?TAUS)).

compareUTC(UTC1,UTC2) ->
	case (UTC1 - UTC2) of
		X when X > 0 -> afterw;
		X when X < 0 -> before;
		X when X == 0 -> concurrent
	end.

now2UTC({MegaSecs, Secs, MicroSecs}) -> 	
	((((MegaSecs * ?MILL) + Secs) * ?TAUS) + (MicroSecs div ?TAUS));
now2UTC(UTCtimestamp) ->
	UTCtimestamp.
	
compareNow({MegaSecs1,Secs1,MicroSecs1},{MegaSecs2,Secs2,MicroSecs2}) ->
	Val1 = MegaSecs1 - MegaSecs2,
	if Val1 > 0 -> afterw;
	   Val1 < 0 -> before;
	   Val1 == 0 -> 
			Val2 = Secs1 - Secs2,
			if Val2 > 0	-> afterw;
			   Val2 < 0 -> before;
			   Val2 == 0 -> 
					Val3 = MicroSecs1 - MicroSecs2,
					if Val3 > 0 -> afterw;
					   Val3 < 0 -> before;
					   Val3 == 0 -> concurrent
					end
			end
	end.

%% -------------------------------------------
%
% Vergleich der Zeitstempel erlang:timestamp()
% beim Nachrichtendienst
% {MegaSecs, Secs, MicroSecs}
% {10^6,10^0,10^(-6)}
% {1000000, 1, 0.000001}
% 
%%
validTS({X,Y,Z}) -> is_integer(X) and
                    is_integer(Y) and
					is_integer(Z);
validTS(_SomethingElse) -> %io:format("***>>>>****>>>>~p<<<<<*****<<<<<\n\n",[SomethingElse]),
                          false.
lessTS({X1,Y1,Z1},{X2,Y2,Z2}) -> 
                    (X2 > X1) or
					((X2 == X1) and (Y2 > Y1)) or
					((X2 == X1) and (Y2 == Y1) and (Z2 > Z1));
lessTS(_Something,_Else) -> false.		
			
lessoeqTS({X1,Y1,Z1},{X2,Y2,Z2}) -> 
                    (X2 > X1) or
					((X2 == X1) and (Y2 > Y1)) or
					((X2 == X1) and (Y2 == Y1) and (Z2 >= Z1));
lessoeqTS(_Something,_Else) -> false.	
				
equalTS({X1,Y1,Z1},{X2,Y2,Z2}) -> 
					((X2 == X1) and (Y2 == Y1) and (Z2 == Z1));
equalTS(_Something,_Else) -> false.		
			
diffTS({X1,Y1,Z1},{X2,Y2,Z2}) ->
                    {X1-X2,Y1-Y2,Z1-Z2};					
diffTS(_Something,_Else) -> {-42,-42,-42}.

now2string({Me,Mo,Mi}) ->
                    {{_Year, Month, Day},{Hour, Minute, Second}} = calendar:now_to_local_time({Me,Mo,Mi}),	
	                Tag = lists:concat([klebe(Day,""),".",klebe(Month,"")," ",klebe(Hour,""),":"]),
	                Tag ++ concat([Minute,Second],":") ++ "," ++ toMilliSeconds(Mi)++"|";
now2string(_SomethingElse) -> "00.00 00:00:00,000|".

now2stringD({Me,Mo,Mi}) ->
                    {{_Year, _Month, _Day},{_Hour, Minute, Second}} = calendar:now_to_local_time({Me,Mo,Mi}),	
	                Tag = lists:concat([klebe(0,""),".",klebe(0,"")," ",klebe(0,""),":"]),
	                Tag ++ concat([Minute,Second],":") ++ "," ++ toMicroSeconds(Mi)++"|";
now2stringD(_SomethingElse) -> "00.00 00:00:00,000|".

toMicroSeconds(MicroSecs) ->
	Seconds = MicroSecs / ?MILL,
	%% Korrektur, da string:substr( float_to_list(0.234567), 3, 3). 345 ergibt
	if (Seconds < 1) -> CorSeconds = Seconds + 1;
	   (Seconds >= 1) -> CorSeconds = Seconds
	end,
	string:substr( float_to_list(CorSeconds), 3, 7).
	
%% -------------------------------------------
%%
% Oeffnen von UDP Sockets, zum Senden und Empfangen 
% Schliessen nicht vergessen: timer:apply_after(?LIFETIME, gen_udp, close, [Socket]),

% openSe(IP,Port) -> Socket
% diesen Prozess PidSend (als NebenlÃ¤ufigenprozess gestartet) bekannt geben mit
%  gen_udp:controlling_process(Socket, PidSend),
% senden  mit gen_udp:send(Socket, Addr, Port, <MESSAGE>)
openSe(Addr, Port) ->
  io:format("~nAddr: ~p~nPort: ~p~n", [Addr, Port]),
  {ok, Socket} = gen_udp:open(Port, [binary, 	{active, false}, {reuseaddr, true}, {ip, Addr}, {multicast_ttl, ?TTL}, inet, 
												{multicast_loop, true}, {multicast_if, Addr}]),
  Socket.

% openRec(IP,Port) -> Socket
% diesen Prozess PidRec (als NebenlÃ¤ufigenprozess gestartet) bekannt geben mit
%  gen_udp:controlling_process(Socket, PidRec),
% aktives Abholen mit   {ok, {Address, Port, Packet}} = gen_udp:recv(Socket, 0),
openRec(MultiCast, Addr, Port) ->
  io:format("~nMultiCast: ~p~nAddr: ~p~nPort: ~p~n", [MultiCast, Addr, Port]),
  {ok, Socket} = gen_udp:open(Port, [binary, 	{active, false}, {reuseaddr, true}, {multicast_if, Addr}, inet, 
												{multicast_ttl, ?TTL}, {multicast_loop, true}, {add_membership, {MultiCast, Addr}}]),
  Socket.

  % Aktives UDP-Socket:
% openSe(IP,Port) -> Socket
% diesen Prozess PidSend (als NebenlÃ¤ufigenprozess gestartet) bekannt geben mit
%  gen_udp:controlling_process(Socket, PidSend),
% senden  mit gen_udp:send(Socket, Addr, Port, <MESSAGE>)
openSeA(Addr, Port) ->
  io:format("~nAddr: ~p~nPort: ~p~n", [Addr, Port]),
  {ok, Socket} = gen_udp:open(Port, [binary, 	{active, true}, {ip, Addr}, inet, 
												{multicast_loop, false}, {multicast_if, Addr}]),
  Socket.
 
% openRec(IP,Port) -> Socket
% diesen Prozess PidRec (als NebenlÃ¤ufigenprozess gestartet) bekannt geben mit
%  gen_udp:controlling_process(Socket, PidRec),
% passives Empfangen mit   receive	{udp, ReceiveSocket, IP, InPortNo, Packet} -> ... end
openRecA(MultiCast, Addr, Port) ->
  io:format("~nMultiCast: ~p~nAddr: ~p~nPort: ~p~n", [MultiCast, Addr, Port]),
  {ok, Socket} = gen_udp:open(Port, [binary, 	{active, true}, {reuseaddr, true}, {multicast_if, Addr}, inet, 
												{multicast_ttl, ?TTL}, {multicast_loop, false}, {add_membership, {MultiCast, Addr}}]),
  Socket.

% Nachrichtenpaket fertig stellen
createBinaryS(Station) ->
    % 1 Byte for Stationtype  
%	<<(list_to_binary(Station)):8/binary>>.
	<<(list_to_binary(Station))/binary>>.
createBinaryD(Data) ->
    % 24 Byte for Payload  
%	<<(list_to_binary(Data)):192/binary>>.
	<<(list_to_binary(Data))/binary>>.
createBinaryNS(NextSlot) ->
    % 1 Byte for NextSlot
%    <<NextSlot:8/integer>>.
    <<NextSlot>>.
createBinaryT(Timestamp) ->    
    % 8 Byte for Time  
    <<(Timestamp):64/big-unsigned-integer>>.	
concatBinary(BinStation,BinData,BinNextSlot,BinTime) ->         
    % Konkatenieren der Binaries: Nachrichtenformat pruefen!             
    <<BinStation/binary, BinData/binary,BinNextSlot/binary,BinTime/binary>>.
concatBinary(BinStation,BinData,BinNextSlot) ->         
    % Konkatenieren der Binaries: Nachrichtenformat pruefen!             
    <<BinStation/binary, BinData/binary,BinNextSlot/binary>>.

message_to_string(Packet)	->
%	Packet= <<BinStationTyp:8/binary,BinNutzdaten:192/binary,Slot:8/integer,Timestamp:64/integer>>
	StationTyp = binary:bin_to_list(Packet,0,1),
    Nutzdaten= binary:bin_to_list(Packet,1,24),
	Slot = binary:decode_unsigned(binary:part(Packet,25,1)),
	Timestamp = binary:decode_unsigned(binary:part(Packet,26,8)),
    {StationTyp,Nutzdaten,Slot,Timestamp}.
	
	
	
%% -------------------------------------------
%
% initialisiert die Mi der ggT-Prozesse, um den
% gewÃ¼nschten ggT zu erhalten.
% Beispielaufruf: bestimme_mis(42,88),
% 42: gewÃ¼nschter ggT
% 88: Anzahl benÃ¶tigter Zahlen
% 
%%
bestimme_mis(WggT,GGTsCount) -> bestimme_mis(WggT,GGTsCount,[]).
bestimme_mis(_WggT,0,Mis) -> Mis;
bestimme_mis(WggT,GGTs,Mis) -> 
	Mi = einmi([2,3,5,7,11,13,17],WggT),
	Enthalten = lists:member(Mi,Mis), 
	if 	Enthalten -> bestimme_mis(WggT,GGTs,Mis);
		true ->	bestimme_mis(WggT,GGTs-1,[Mi|Mis])
	end.	
% berechnet ein Mi
einmi([],Akku) -> Akku;	
einmi([Prim|Prims],Akku) ->
	Expo = rand:uniform(3)-1, % 0 soll mÃ¶glich sein!
	AkkuNeu = trunc(Akku * math:pow(Prim,Expo)), % trunc erzeugt integer, was fÃ¼r rem wichtig ist
	einmi(Prims,AkkuNeu).	

testeMI(WggT,GGTsCount) ->
		testeMis(bestimme_mis(WggT,GGTsCount),WggT).

testeMis([],_WggT) -> true;
testeMis([Num1|Rest],WggT) ->
	Val = Num1 rem WggT,
	case Val of
		0 -> testeMis(Rest,WggT);
		_X -> io:format("Zahl ~p Rest ~p\n",[Num1,Val]),testeMis(Rest,WggT)
	end.
