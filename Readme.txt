--------------------
Starten der Nodes:
--------------------
(w)erl -(s)name <KoordinatorName> -setcookie zummsel
(w)erl -(s)name <NameserviceName> -setcookie zummsel
(w)erl -(s)name <ggTPName> -setcookie zummsel

Starten des Namensdienstes:
--------------------------
1>nameservice:start( ).

Starten des Koordinators:
--------------------------
2>koordinator:start( ).

% liest die koordinator.cfg ein:
% {arbeitszeit, 3}:					simulierte Arbeitszeit f¸r die ggT-Berechnung
% {termzeit, 3}:					Wenn termzeit lang keine Berechnung dann wird Terminierungsabstimmung initiiert
% {ggtprozessnummer, 42}:			Anzahl ggT Prozesse je Starter (und default ggT)
% {nameservicenode, 'auskunft@lab33.cpt.haw-hamburg.de'}:	node des Namensdienstes
% {koordinatorname, euklid}:		Name des Koordinators
% {quote, 80}:						Abstimmungsquote fuer Terminierung
% {korrigieren, 0}:					Korigiert falsche Terminierungsmeldungen (== 1 korrigieren, == 0 nicht korrigieren)

Zum Senden an den Koordinator:
{<koordinatorname>, ’<koordinatorname>@<node>’ ! <State>
- State: einen der Nachrichten help, vals (Steuerungswerte), ggt (Zufalls ggT), {ggt,Wggt}, step (Beendet Anmeldephase), nudge (eine Art ping), prompt (aktuelle Mi's), reset oder kill
  prompt: Frage nach Mi;  nudge (schubsen): Lebenszustand
  vals: Steuerwerte; ggt: Zufallswunschggt
- und liest koordinator.cfg ein

Starten der ggT-Prozesse:
--------------------------
3>starter:start(StarterID).

% ggt_starter liest die ggt.cfg ein:
% {praktikumsgruppe, 4}:			Nummer der Praktikumsgruppe
% {teamnummer, 88}:					Nummer des Teams
% {nameservicenode, auskunft@@lab33.cpt.haw-hamburg.de}:		node des Namensdienstes
% {koordinatorname, euklid}:		Name des Koordinators

Runterfahren:
-------------
2> Ctrl/Strg Shift G
-->q

Informationen zu Prozessen bzw. Modulen:
-------------
2> pman:start().
2> process_info(PID).
2> <Module>:module_info().