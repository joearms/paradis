-module(client1).

%% -compile(export_all).

-export([test/0]).

test() ->
    {ok, Pid} = simple_client_server:connect_to_server("localhost", 5123, hello_joe), 
    Msg = {echo, hello},
    io:format("Client1 :: sending:~p to server~n",[Msg]),
    Pid ! {message, Msg},
    receive
	{Pid, message, X} ->
	    io:format("Client1 :: Server responded :: ~p~n",[X])
    end,
    Pid ! close,
    io:format("Client1 :: test completed~n").


	    
