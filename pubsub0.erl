-module(pubsub0).
-compile(export_all).

start() ->
    stop(),
    register(pubsub0, spawn(pubsub0, init, [])).

stop() ->
    (catch exit(whereis(pubsub0), kill)),
    ok.

publish(G, M) -> pubsub0 ! {to_group,G,{publish, M}}.

subscribe(G) -> pubsub0 ! {to_group,G,{subscribe, self()}}.

init() ->
    pubsub_loop([]).

pubsub_loop(L) ->
    receive
	{to_group, Group, Msg} ->
	    case find_group(Group, L) of
		{yes, Pid} ->
		    Pid ! Msg,
		    pubsub_loop(L);
		no ->
		    Pid = spawn(pubsub, agent_loop, [Group,[]]),
		    Pid ! Msg,
		    L1 = [{Group,Pid}|L],
		    pubsub_loop(L1)
	    end
    end.

agent_loop(Group, Pids) ->
    receive
	{subscribe, My_pid} ->
	    agent_loop(Group, [My_pid|Pids]);
	{publish, Msg} ->
	    [Pid ! Msg || Pid <- Pids],	    
	    agent_loop(Group, Pids)
    end.

find_group(Group, L) ->
    case lists:keysearch(Group, 1, L) of
	{value,{_,Val}} -> {yes, Val};
	false           -> no
    end.
