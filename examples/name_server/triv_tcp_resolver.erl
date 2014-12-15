-module(triv_tcp_resolver).
-export([store/2, lookup/1]).

%% -define(HOST, "localhost").
-define(HOST, "77.238.55.150").
-define(PORT, 6000).

store(Key, Value) ->
    case gen_tcp:connect(?HOST, ?PORT,
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

lookup(Key) ->
    case gen_tcp:connect(?HOST, ?PORT,
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
    

