-module(server3).

-export([start/0]).

start() ->
    register(?MODULE, spawn(fun start_server/0)).

start_server() ->
    simple_client_server:start_server(5123, fun connection_attempt/3).

connection_attempt(Pid, Peer, Init) ->
    io:format("Connection from:~p Init:~p~n",[Peer, Init]),
    Pid ! accept,
    server_loop(Pid).

server_loop(Pid) ->
    receive
	{Pid, message, {rpc, R}} ->
	    Reply = do(R),
	    Pid ! {message, Reply},
	    server_loop(Pid);
	{Pid, closed} ->	
	    true;
	Other ->
	    io:format("Server3 :: Unexpected message:~p~n",[Other]),
	    server_loop(Pid)
    end.

do({ls,Wildcard})   -> filelib:wildcard(Wildcard);
do({get_file,File}) -> file:read_file(File);
do({fac,N})         -> fac(N).

fac(0) -> 1;
fac(N) -> N * fac(N-1).

    
