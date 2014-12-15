-module(triv_tcp_fac_client).

-export([fac/1]).

fac(N) ->
    %% ask the name server where the fac server is
    case triv_tcp_resolver:lookup("fac") of
	{ok, {value, {"fac", {Host, Port}}}} ->
	    %% connect to the fac server
	    case gen_tcp:connect(Host, Port,
				 [binary,{packet,4},
				  {active,true}]) of
		{ok, Socket} ->
		    %% encode and send request
		    Reply = term_to_binary({fac_query,N}), 
		    ok = gen_tcp:send(Socket, Reply),
		    receive
			{tcp, Socket, Data} ->
			    %% receive and decode reply
			    gen_tcp:close(Socket),
			    {ok, binary_to_term(Data)}
		    end;
		{error, _} ->
		    {error, connect}
	    end;
	{error, _} ->
	    {error, resolve}
    end.
