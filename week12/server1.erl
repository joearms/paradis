-module(server1).

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
	{Pid, message, {echo, R}=Msg} ->
	    io:format("Server1 :: received:~p~n",[Msg]),
	    io:format("Server1 :: sending reply:~p~n",[Msg]),
	    Pid ! {message, {echoed, R}},
	    server_loop(Pid);
	{Pid, closed} ->	    
	    io:format("Server1 :: client has closed the connection~n",[]);
	Other ->
	    io:format("Server1 :: Unexpected message:~p~n",[Other]),
	    server_loop(Pid)
    end.

