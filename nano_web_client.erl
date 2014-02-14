-module(nano_web_client).
-compile(export_all).

nano_get_url() ->
    nano_get_url("www.google.com").

nano_get_url(Host) ->
    {ok,Socket} = gen_tcp:connect(Host,80,
				  [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"), 
    receive_data(Socket, []).

receive_data(Socket, SoFar) ->
    receive
	{tcp,Socket,Bin} ->  
	    receive_data(Socket, [Bin|SoFar]);
	{tcp_closed,Socket} -> 
	    list_to_binary(lists:reverse(SoFar)) 
    end.
