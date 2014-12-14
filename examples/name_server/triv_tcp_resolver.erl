-module(triv_tcp_resolver).
-export([store/4, lookup/3]).

store(Key, Value,
	 NameServerHost, NameServerPort) ->
    case gen_tcp:connect(NameServerHost, NameServerPort,
			 [binary,{packet,4},
			  {active,true}]) of
	{ok, Socket} ->
	    ok = gen_tcp:send(Socket, 
			      term_to_binary({add,
					      Key,Value})),
	    gen_tcp:close(Socket),
	    {ok, sent};
	{error, _} ->
	    {error, connect}
    end.

lookup(Key, NameServerHost, NameServerPort) ->     
    case gen_tcp:connect(NameServerHost, NameServerPort,
			 [binary,{packet,4},
			  {active,true}]) of
	{ok, Socket} ->
	    ok = gen_tcp:send(Socket, 
			      term_to_binary({lookup,Key})),
	    receive
		{tcp, Socket, Data} ->
		    gen_tcp:close(Socket),
		    {ok, binary_to_term(Data)};
		{tcp_closed, Socket} ->
		    {error, closed}
	    end;
	{error, _} ->
	    {error, connect}
    end.
    

