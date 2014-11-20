-module(client3).
-export([test/0]).

-import(simple_client_server, [rpc/2, connect_to_server/3]).

test() ->
    {ok, Pid} = connect_to_server("localhost", 5123, hello_joe), 
    Val = rpc(Pid, {fac,50}),
    io:format("Fac(50) = ~p~n",[Val]),
    Files = rpc(Pid, {ls,"*.erl"}),
    io:format("Files=~p~n",[Files]),
    Data = rpc(Pid, {get_file, "server3.erl"}),
    io:format("server3.erl = ~p~n",[Data]),
    Pid ! close,
    io:format("Client3 :: test completed~n").

	    
