-module(server2).

-export([start/0]).

start() ->
    register(?MODULE, spawn(fun start_server1/0)).

start_server1() ->
    simple_client_server:start_server(5123, fun connection_attempt/3).

connection_attempt(Pid, Peer, Init) ->
    io:format("Connection from:~p Init:~p~n",[Peer, Init]),
    Pid ! accept,
    server_loop(Pid).

server_loop(Pid) ->
    receive
	{Pid, message, {rpc, R}} ->
	    io:format("Server2 :: command:~p~n",[R]),
	    Reply = do(R),
	    io:format("Server1 :: sending reply:~p~n",[Reply]),
	    Pid ! {message, Reply},
	    server_loop(Pid);
	{Pid, closed} ->	    
	    io:format("Server2 :: client has closed the connection~n",[]);
	Other ->
	    io:format("Server2 :: Unexpected message:~p~n",[Other]),
	    server_loop(Pid)
    end.

do({fac,N}) -> fac(N).

fac(0) -> 1;
fac(N) -> N * fac(N-1).

    
