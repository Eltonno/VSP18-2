-module(ggt_process).
-export([start/9, start_ggt_process/1, reset_terminate_timer/2, maybe_update_mi/3, update_neighbours/4, set_pm/4, term_request/2, voting_response/3, maybe_send_brief_term/2]).

start_ggt_process(ActVal) ->
  GgTNumber = maps:get(ggtnumber, ActVal),
  Coordinator = maps:get(coordinator, ActVal),
  Praktikumsgruppe = maps:get(praktikumsgruppe, ActVal),
  GroupNumber = maps:get(groupnumber, ActVal),
  StartNumber = maps:get(startnumber, ActVal),
  util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), integer_to_list(GgTNumber) ++ " starttime: " ++ util:timeMilliSecond() ++ " with PID " ++ pid_to_list(self()) ++ " on " ++ atom_to_list(node()) ++ "\n"),
  GgTName = list_to_atom(integer_to_list(Praktikumsgruppe) ++ integer_to_list(GroupNumber) ++ integer_to_list(GgTNumber) ++ integer_to_list(StartNumber)),
  register(GgTName, self()),
  util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "registered locally\n"),
  NameService = global:whereis_name(nameservice),
  NameService ! {self(), {rebind, GgTName, node()}},
  receive
    ok -> util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "registered at nameservice\n")
  end,
  Coordinator ! {hello, GgTName},
  util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "registered at coordinator\n"),
  TermTime = maps:get(termtime, ActVal) * 1000,
  {ok, TRef} = timer:apply_after(TermTime, ?MODULE, term_request, [ActVal, GgTName]),
  NewActVal = maps:update(terminateTimer, TRef, ActVal),
  handle_messages(NewActVal, GgTName, GgTName).

reset_terminate_timer(ActVal, GgTName) ->
 %% GgTNumber = maps:get(ggtnumber, ActVal),
  timer:cancel(maps:get(terminateTimer, ActVal)),
  TermTime = maps:get(termtime, ActVal) * 1000,
  {ok, TRef} = timer:apply_after(TermTime, ?MODULE, term_request, [ActVal, GgTName]),
  UpdatedTerm = maps:update(isTerminating, false, ActVal),
  UpdatedTimestamp = maps:update(lastNumberReceived, erlang:timestamp(), UpdatedTerm),
  maps:update(terminateTimer, TRef, UpdatedTimestamp).


bind_ggt(GgTNumber, ActVal) ->
  maps:get(nameservice, ActVal) ! {self(), {lookup, GgTNumber}},
  receive
    not_found ->
      util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "Warning: Could not find the ggT process '" ++ integer_to_list(GgTNumber) ++ "'\n");
    {pin, GgTPID} ->
      GgTPID
  end.

update_neighbours(Left, Right, ActVal, GgTNumber) ->
  util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "left neighbour registered: " ++ atom_to_list(Left) ++ "\n"),
  LeftPID = bind_ggt(Left, ActVal),
  util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "left neighbour bound\n"),
  util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "right neighbour registered: " ++ atom_to_list(Right) ++ "\n"),
  RightPID = bind_ggt(Right, ActVal),
  util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "right neighbour bound\n"),
  UpdatedNeighbourNamesActVal = maps:update(rightneigbor, Right, maps:update(leftneighbor, Left, ActVal)),
  maps:update(leftneighborPID, LeftPID, maps:update(rightneighborPID, RightPID, UpdatedNeighbourNamesActVal)).

set_pm(Mi, ActVal, GgTNumber, GgTName) ->
  util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "setpm: " ++ integer_to_list(Mi) ++ "\n"),
  NewActVal = reset_terminate_timer(ActVal, GgTName),
  maps:update(mi, Mi, NewActVal).

maybe_update_mi(Y, ActVal, GgTName) ->
  Mi = maps:get(mi, ActVal),
  L = maps:get(leftneighborPID, ActVal),
  R = maps:get(rightneighborPID, ActVal),
  Coordinator = maps:get(coordinator, ActVal),
  GgTNumber = maps:get(ggtnumber, ActVal),
  if
    Y < Mi ->
      timer:sleep(maps:get(workingtime, ActVal) * 1000),
      NewMi = ((Mi - 1) rem Y) + 1,
      L ! {sendy, NewMi},
      R ! {sendy, NewMi},
      Coordinator ! {briefmi, {GgTName, NewMi, erlang:timestamp()}},
      util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "sendy: " ++ integer_to_list(Y) ++ " (" ++ integer_to_list(Mi) ++ "); new mi: " ++ integer_to_list(NewMi) ++ " " ++ util:timeMilliSecond() ++ "\n"),
      maps:update(mi, NewMi, reset_terminate_timer(ActVal, GgTName));
    true ->
      util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "sendy: " ++ integer_to_list(Y) ++ " (" ++ integer_to_list(Mi) ++ "); no new mi\n"),
      reset_terminate_timer(ActVal, GgTName)
  end.

term_request(ActVal, GgTName) ->
  IsTerminating = maps:get(isTerminating, ActVal),
  if
    IsTerminating ->
      ok;
    true ->
      GgTNumber = maps:get(ggtnumber, ActVal),
      util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "Start termination voting " ++ util:timeMilliSecond() ++ "\n"),
      NameService = global:whereis_name(nameservice),
      NameService ! {self(), {multicast, vote, GgTName}}
  end.

-spec voting_response(atom(), map(), atom()) -> atom().
voting_response(GgTNumber, ActVal, GgTName) ->
  Threshold = round(maps:get(termtime, ActVal) / 2),
  util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "DEBUG: voting response | threshold: " ++ integer_to_list(Threshold) ++ "\n"),
  LastNumberReceived = maps:get(lastNumberReceived, ActVal),
  if
    LastNumberReceived =:= 0 ->
      util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "Voting no for term request with ignoring\n"),
      ok;
    true ->
      PassedTime = round(timer:now_diff(erlang:timestamp(), maps:get(lastNumberReceived, ActVal)) / 1000000),
      util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "DEBUG: voting response | passed time: " ++ integer_to_list(PassedTime) ++ "\n"),
      if
        PassedTime > Threshold ->
          maps:get(nameservice, ActVal) ! {self(), {lookup, GgTName}},
          receive
            not_found ->
              util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "Warning: Could not find the ggT process '" ++ integer_to_list(GgTNumber) ++ "'\n");
            {pin, Initiator} ->
              util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "Sending voteYes to " ++ integer_to_list(GgTNumber) ++ "\n"),
              Initiator ! {voteYes, GgTName}
          end;
        true ->
          util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "Voting no for term request with ignoring\n"),
          ok
      end
  end.

maybe_send_brief_term(GgTNumber, ActVal) ->
  CurrentVotes = maps:get(yesVotes, ActVal),
  Quota = maps:get(quota, ActVal),
  NewVotes = CurrentVotes + 1,
  util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "received yes vote from " ++ integer_to_list(GgTNumber) ++ " with a total votes of " ++ integer_to_list(NewVotes) ++ " " ++ util:timeMilliSecond() ++ "\n"),
  NewActVal = maps:update(yesVotes, NewVotes, ActVal),
  if
    NewVotes == Quota ->
      Coordinator = maps:get(coordinator, ActVal),
      Coordinator ! {self(), briefterm, {maps:get(ggtnumber, ActVal), maps:get(mi, ActVal), erlang:timestamp()}},
      NewTermsCount = maps:get(terminatedCalculations, NewActVal) + 1,
      util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "Send #" ++ integer_to_list(NewTermsCount) ++ " terminated brief to coordinator\n"),
      maps:update(terminatedCalculations, NewTermsCount, maps:update(yesVotes, 0, NewActVal));
    true ->
      NewActVal
  end.

handle_messages(ActVal, GgTNumber, GgTName) ->
  receive
  % Sets the right and left neighbour processes for the recursive algorithm
    {setneighbors, LeftNeighbour, RightNeighbour} ->
      handle_messages(update_neighbours(LeftNeighbour, RightNeighbour, ActVal, GgTNumber), GgTNumber, GgTName);
  % Sets a new Mi to calculate
    {setpm, NewMi} ->
      handle_messages(set_pm(NewMi, ActVal, GgTNumber, GgTName), GgTNumber, GgTName);
  % Starts the algorithm to calculate a ggT if possible
    {sendy, Y} ->
      handle_messages(maybe_update_mi(Y, ActVal, GgTName), GgTNumber, GgTName);
  % Starts the voting process answer
    {_From, {vote, Initiator}} ->
      voting_response(Initiator, ActVal, GgTName),
      handle_messages(ActVal, GgTNumber, GgTName);
  % Sends brief mi to coordinator if enough yes votes came in
    {voteYes, Name} ->
      handle_messages(maybe_send_brief_term(Name, ActVal), GgTNumber, GgTName);
  % Used for getting status
    {From, tellmi} ->
      From ! {mi, maps:get(mi, ActVal)},
      handle_messages(ActVal, GgTNumber, GgTName);
  % Used for getting status
    {From, pingGGT} ->
      From ! {pongGGT, maps:get(ggtnumber, ActVal)},
      handle_messages(ActVal, GgTNumber, GgTName);
    kill ->
      util:logging(list_to_atom("GGTP_" ++ integer_to_list(GgTNumber) ++ "@" ++ atom_to_list(node()) ++ ".log"), "shutting down ggt\n"),
      exit(self(), normal),
      ok
  end.

start(WorkingTime, TerminationTime, Praktikumsgruppe, GroupNumber, GgTNumber, StartNumber, NameService, Coordinator, Blubb) ->
  ActVal = #{
    ggtnumber => GgTNumber,
    workingtime => WorkingTime,
    termtime => TerminationTime,
    coordinator => Coordinator,
    nameservice => NameService,
    praktikumsgruppe => Praktikumsgruppe,
    groupnumber => GroupNumber,
    startnumber => StartNumber,
    leftneighbor => undefined,
    rightneigbor => undefined,
    leftneighborPID => undefined,
    rightneighborPID => undefined,
    mi => undefined,
    yesVotes => 0,
    terminateTimer => undefined,
    lastNumberReceived => 0,
    isTerminating => false,
    terminatedCalculations => 0
  },
  spawn(?MODULE, start_ggt_process, [ActVal]).
