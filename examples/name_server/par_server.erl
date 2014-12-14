-module(par_server).
-export([start_parallel_server/1]).

start_parallel_server(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [options]),
    spawn(fun() -> connect(Listen) end).

connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> connect(Listen) end),
    loop(Socket).

loop(Socket) ->
    receive
	{tcp, Socket, Bin} ->
	    do_something,
	    loop(Socket);
	{tcp_closed, Socket} ->
	    do_something_else
    end.
