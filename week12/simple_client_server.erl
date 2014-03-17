-module(simple_client_server).

%% -compile(export_all).

-export([start_server/2, connect_to_server/3, rpc/2]).

start_server(Port, Fun) ->
    spawn(fun() -> 
		  init_server(Port, Fun),
		  receive
		  after infinity -> true
		  end
	  end).

init_server(Port, Fun) ->
    case gen_tcp:listen(Port,
			[binary,{packet,4},{reuseaddr,true},{active,true}]) of
	{ok, Listen} ->
	    IP = my_ip(),
	    io:format("~p listening on port:~p~n",[IP, Port]),
	    spawn(fun() ->  par_connect(Listen, Fun) end);
	Error ->
	    io:format("Cannot open port:~p Error:~p~n",[Port,Error])
    end.

par_connect(Listen, Fun) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    {ok, Peer} = inet:peername(Socket),
    io:format("Got a connection from:~p~n",[Peer]),
    spawn(fun() ->  par_connect(Listen, Fun) end),
    handshake(Socket, Peer, Fun).

handshake(Socket, Peer, Fun) ->
    receive
	{tcp, Socket, Data} ->
	    Erl = binary_to_term(Data),
	    S = self(),
	    {Pid,_Ref} = spawn_monitor(fun() -> Fun(S, Peer, Erl) end),
	    receive
		accept ->
		    gen_tcp:send(Socket, term_to_binary(accept)),
		    middle_man(Pid, Socket);
		reject ->
		    gen_tcp:send(Socket, term_to_binary(reject)),
		    gen_tcp:close(Socket)
	    end
    end.

middle_man(Pid, Socket) ->
    receive
	{tcp, Socket, Bin} ->
	    Term = binary_to_term(Bin),
	    Pid ! {self(), message, Term},
	    middle_man(Pid, Socket);
	{tcp_closed, Socket} ->
	    Pid ! {self(), closed};
	close ->
	    gen_tcp:close(Socket);
	{message, Term}->
	    Bin = term_to_binary(Term),
	    gen_tcp:send(Socket, Bin),
	    middle_man(Pid, Socket);
	X ->
	    io:format("MM :: Unexpected message dropped:~p~n",[X]),
	    middle_man(Pid, Socket)
    end.

connect_to_server(Host, Port, Init) ->
    S = self(),
    Pid = spawn(fun() -> connect_to_server(S, Host, Port, Init) end),
    receive
	{Pid, accept} ->
	    {ok, Pid};
	{Pid, Error} ->
	    Error
    end.
	
connect_to_server(Parent, Host, Port, Init) ->
    case gen_tcp:connect(Host, Port, [binary,{packet,4}]) of
	{ok, Socket} ->
	    %% handhake
	    gen_tcp:send(Socket, term_to_binary(Init)),
	    receive
		{tcp, Socket, Bin} ->
		    case binary_to_term(Bin) of
			accept ->
			    Parent ! {self(), accept},
			    middle_man(Parent, Socket);
			reject ->
			    Parent ! {self(), {error, rejected}};
			Other ->
			    Parent ! {self(), {error, protocol_error}}
		    end;
		{tcp_closed, Socket} ->
		    Parent ! {self(), {error, handshake_not_completed}};
		X ->
		    io:format("Unexpected message:~p~n",[X])
	    end;
	_ ->
	    Parent ! {self(), {error, e_no_connection}},
	    io:format("Cannot connect:~n")
    end.
	    
my_ip() ->
    case inet:ifget("eth0",[addr]) of
        {ok,[{addr,IP}]} ->
            IP;
        _ ->
            exit(cannot_find_local_ip)
    end.

rpc(Pid, Query) ->
    Msg = {rpc, Query},
    Pid ! {message, Msg},
    receive
	{Pid, message, X} ->
	    X
    after 1000 ->
	    timeout
    end.
    
