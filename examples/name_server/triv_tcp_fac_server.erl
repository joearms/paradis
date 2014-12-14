-module(triv_tcp_fac_server).

-export([start/0, start/1]).

start() ->
    start(6010).

start(Port) ->
    io:format("starting factorial server on port ~p~n",[Port]),
    {ok, Listen} = gen_tcp:listen(Port,
				  [binary,{packet,4},
                                   {reuseaddr,true},
                                   {active,true}]),
    %% Now register ourselves with the name server
    triv_tcp_resolver:store("fac", {"localhost", Port},
			    "localhost", 6000),
    spawn(fun() -> connect(Listen) end).

connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> connect(Listen) end),
    connection_loop(Socket).

connection_loop(Socket) ->
    receive
	{tcp, Socket, Bin} ->
	    Term = binary_to_term(Bin),
	    case Term of
		{fac_query, N} ->
		    Reply = {faq_response, fac(N)},
		    gen_tcp:send(Socket, 
				 term_to_binary(Reply)),
		    connection_loop(Socket)
	    end;
	{tcp_closed, Socket} ->
	    exit(normal)
    end.

fac(0) -> 1;
fac(N) -> N * fac(N-1).
