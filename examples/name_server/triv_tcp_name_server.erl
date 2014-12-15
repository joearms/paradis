-module(triv_tcp_name_server).

-export([start/0]).

start() -> start(6000).

start(Port) ->
    IP = elib:my_ip(),
    io:format("Starting name server ~p:~p~n",[IP, Port]),
    NameServerPid = spawn(fun() -> name_server_loop([]) end),
    {ok, Listen} = gen_tcp:listen(Port, 
				  [binary,{packet,4},
                                   {reuseaddr,true},
                                   {active,true}]),
    spawn(fun() -> connect(Listen, NameServerPid) end).

connect(Listen, NameServerPid) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> connect(Listen, NameServerPid) end),
    connection_loop(Socket, NameServerPid).

connection_loop(Socket, NameServerPid) ->
    receive
	{tcp, Socket, Bin} ->
	    Term = binary_to_term(Bin),
	    case Term of
		{add, Key, Value} ->
		    io:format("Adding:~p => ~p~n",
			      [Key,Value]),
		    NameServerPid ! {add, Key, Value};
		{lookup, Key} ->
		    io:format("Lookup:~p~n",[Key]),
		    Facts = rpc(NameServerPid, {lookup, Key}),
		    io:format("Reply:~p~n",[Facts]),
		    gen_tcp:send(Socket, term_to_binary(Facts))
	    end,
	    connection_loop(Socket, NameServerPid);
	{tcp_closed, Socket} ->
	    exit(normal)
    end.

%%----------------
%% The name server

name_server_loop(L) ->
    receive
	{add, Key, Value} ->
	    name_server_loop([{Key,Value}|L]);
	{From, {lookup, Key}} ->
	    Value = lists:keysearch(Key, 1, L),
	    From ! {self(), Value},
	    name_server_loop(L)
    end.
		 
%%---------------------------
%% rpc should be in a library

rpc(Pid, Query) ->
    Self = self(),
    Pid ! {Self, Query},
    receive
	{Pid, Reply} ->
	    Reply
    end.
		
