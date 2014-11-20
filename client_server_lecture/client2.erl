-module(client2).
-export([test/0]).

test() ->
    {ok, Pid} = simple_client_server:connect_to_server("localhost", 5123, hello_joe), 
    Val = rpc(Pid, {fac,50}),
    io:format("Fac(50) = ~p~n",[Val]),
    Pid ! close,
    io:format("Client2 :: test completed~n").


rpc(Pid, Query) ->
    Msg = {rpc, Query},
    io:format("Client2 :: sending:~p to server~n",[Msg]),
    Pid ! {message, Msg},
    receive
	{Pid, message, X} ->
	    io:format("Client2 :: Server responded :: ~p~n",[X]),
	    X
    end.


	    
