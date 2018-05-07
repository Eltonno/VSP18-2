-module(ggt).
-export([start/6, start_ggt_process/1, reset_terminate_timer/1, maybe_update_mi/2, update_neighbours/3, set_pm/2, term_request/1, voting_response/2, maybe_send_brief_term/2]).

-spec log(map(), list()) -> atom().
log(Config, Message) ->
  GgTName = maps:get(ggtname, Config),
  Logfile = list_to_atom(lists:concat(["GGTP_", atom_to_list(GgTName), "@", atom_to_list(node()), ".log"])),
  FullMessage = Message ++ ["\n"],
  util:logging(Logfile, lists:concat(FullMessage)),
  Logfile.

start_ggt_process(ActVal) ->
  GgTName = maps:get(ggtname, ActVal),
  Coordinator = maps:get(coordinator, ActVal),
  log(ActVal, [atom_to_list(GgTName), " starttime: ", util:timeMilliSecond(), " with PID ", pid_to_list(self()), " on ", atom_to_list(node())]),
  register(GgTName, self()),
  log(ActVal, ["registered locally"]),
  maps:get(nameservice, ActVal) ! {self(), {rebind, GgTName, node()}},
  receive
    ok -> log(ActVal, ["registered at nameservice"])
  end,
  Coordinator ! {hello, GgTName},
  log(ActVal, ["registered at coordinator"]),
  TermTime = maps:get(termtime, ActVal) * 1000,
  {ok, TRef} = timer:apply_after(TermTime, ?MODULE, term_request, [ActVal]),
  NewActVal = maps:update(terminateTimer, TRef, ActVal),
  handle_messages(NewActVal).

reset_terminate_timer(ActVal) ->
  timer:cancel(maps:get(terminateTimer, ActVal)),
  TermTime = maps:get(termtime, ActVal) * 1000,
  {ok, TRef} = timer:apply_after(TermTime, ?MODULE, term_request, [ActVal]),
  UpdatedTerm = maps:update(isTerminating, false, ActVal),
  UpdatedTimestamp = maps:update(lastNumberReceived, erlang:timestamp(), UpdatedTerm),
  maps:update(terminateTimer, TRef, UpdatedTimestamp).


bind_ggt(GgTName, ActVal) ->
  maps:get(nameservice, ActVal) ! {self(), {lookup, GgTName}},
  receive
    not_found ->
      log(ActVal, ["Warning: Could not find the ggT process '", atom_to_list(GgTName), "'"]);
    {pin, GgTPID} ->
      GgTPID
  end.

update_neighbours(Left, Right, ActVal) ->
  log(ActVal, ["left neighbour registered: ", atom_to_list(Left)]),
  LeftPID = bind_ggt(Left, ActVal),
  log(ActVal, ["left neighbour bound"]),
  log(ActVal, ["right neighbour registered: ", atom_to_list(Right)]),
  RightPID = bind_ggt(Right, ActVal),
  log(ActVal, ["right neighbour bound"]),
  UpdatedNeighbourNamesActVal = maps:update(rightneigbor, Right, maps:update(leftneighbor, Left, ActVal)),
  maps:update(leftneighborPID, LeftPID, maps:update(rightneighborPID, RightPID, UpdatedNeighbourNamesActVal)).

set_pm(Mi, ActVal) ->
  log(ActVal, ["setpm: ", integer_to_list(Mi)]),
  NewActVal = reset_terminate_timer(ActVal),
  maps:update(mi, Mi, NewActVal).

maybe_update_mi(Y, ActVal) ->
  Mi = maps:get(mi, ActVal),
  L = maps:get(leftneighborPID, ActVal),
  R = maps:get(rightneighborPID, ActVal),
  Coordinator = maps:get(coordinator, ActVal),
  GgTName = maps:get(ggtname, ActVal),
  if
    Y < Mi ->
      timer:sleep(maps:get(workingtime, ActVal) * 1000),
      NewMi = ((Mi - 1) rem Y) + 1,
      L ! {sendy, NewMi},
      R ! {sendy, NewMi},
      Coordinator ! {briefmi, {GgTName, NewMi, erlang:now()}},
      log(ActVal, ["sendy: ", integer_to_list(Y), " (", integer_to_list(Mi), "); new mi: ", integer_to_list(NewMi), " ", util:timeMilliSecond()]),
      maps:update(mi, NewMi, reset_terminate_timer(ActVal));
    true ->
      log(ActVal, ["sendy: ", integer_to_list(Y), " (", integer_to_list(Mi), "); no new mi"]),
      reset_terminate_timer(ActVal)
  end.

term_request(ActVal) ->
  IsTerminating = maps:get(isTerminating, ActVal),
  if
    IsTerminating ->
      ok;
    true ->
      log(ActVal, ["Start termination voting ", util:timeMilliSecond()]),
      maps:get(nameservice, ActVal) ! {self(), {multicast, vote, maps:get(ggtname, ActVal)}}
  end.

-spec voting_response(atom(), map()) -> atom().
voting_response(GgTName, ActVal) ->
  Threshold = round(maps:get(termtime, ActVal) / 2),
  log(ActVal, ["DEBUG: voting response | threshold: ", integer_to_list(Threshold)]),
  LastNumberReceived = maps:get(lastNumberReceived, ActVal),
  if
    LastNumberReceived =:= 0 ->
      log(ActVal, ["Voting no for term request with ignoring"]),
      ok;
    true ->
      PassedTime = round(timer:now_diff(erlang:timestamp(), maps:get(lastNumberReceived, ActVal)) / 1000000),
      log(ActVal, ["DEBUG: voting response | passed time: ", integer_to_list(PassedTime)]),
      if
        PassedTime > Threshold ->
          maps:get(nameservice, ActVal) ! {self(), {lookup, GgTName}},
          receive
            not_found ->
              log(ActVal, ["Warning: Could not find the ggT process '", atom_to_list(GgTName), "'"]);
            {pin, Initiator} ->
              log(ActVal, ["Sending voteYes to ", atom_to_list(GgTName)]),
              Initiator ! {voteYes, maps:get(ggtname, ActVal)}
          end;
        true ->
          log(ActVal, ["Voting no for term request with ignoring"]),
          ok
      end
  end.

maybe_send_brief_term(GgTName, ActVal) ->
  CurrentVotes = maps:get(yesVotes, ActVal),
  Quota = maps:get(quota, ActVal),
  NewVotes = CurrentVotes + 1,
  log(ActVal, ["received yes vote from ", atom_to_list(GgTName), " with a total votes of ", integer_to_list(NewVotes), " ", util:timeMilliSecond()]),
  NewActVal = maps:update(yesVotes, NewVotes, ActVal),
  if
    NewVotes == Quota ->
      Coordinator = maps:get(coordinator, ActVal),
      Coordinator ! {self(), briefterm, {maps:get(ggtname, ActVal), maps:get(mi, ActVal), erlang:now()}},
      NewTermsCount = maps:get(terminatedCalculations, NewActVal) + 1,
      log(ActVal, ["Send #", integer_to_list(NewTermsCount), " terminated brief to coordinator"]),
      maps:update(terminatedCalculations, NewTermsCount, maps:update(yesVotes, 0, NewActVal));
    true ->
      NewActVal
  end.

handle_messages(ActVal) ->
  receive
  % Sets the right and left neighbour processes for the recursive algorithm
    {setneighbors, LeftNeighbour, RightNeighbour} ->
      handle_messages(update_neighbours(LeftNeighbour, RightNeighbour, ActVal));
  % Sets a new Mi to calculate
    {setpm, NewMi} ->
      handle_messages(set_pm(NewMi, ActVal));
  % Starts the algorithm to calculate a ggT if possible
    {sendy, Y} ->
      handle_messages(maybe_update_mi(Y, ActVal));
  % Starts the voting process answer
    {_From, {vote, Initiator}} ->
      voting_response(Initiator, ActVal),
      handle_messages(ActVal);
  % Sends brief mi to coordinator if enough yes votes came in
    {voteYes, Name} ->
      handle_messages(maybe_send_brief_term(Name, ActVal));
  % Used for getting status
    {From, tellmi} ->
      From ! {mi, maps:get(mi, ActVal)},
      handle_messages(ActVal);
  % Used for getting status
    {From, pingGGT} ->
      From ! {pongGGT, maps:get(ggtname, ActVal)},
      handle_messages(ActVal);
    kill ->
      log(ActVal, ["shutting down ggt"]),
      exit(self(), normal),
      ok
  end.


%% Starts the ggT Process and registers at the coordinator, nameservice and locally at the node
start(WorkingTime, TerminationTime, Quota, GgTName, Coordinator, NameService) ->
  ActVal = #{
    ggtname => GgTName,
    workingtime => WorkingTime,
    termtime => TerminationTime,
    quota => Quota,
    coordinator => Coordinator,
    nameservice => NameService,
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
