-module(bug).

-compile(export_all).

test1(Port) ->
    case gen_tcp:listen(Port,
			[binary,{packet,4},{reuseaddr,true},{active,true}]) of
	{ok, Listen} ->
	    {ok, S} = gen_tcp:accept(Listen),
	    io:format("Socket:~p~n",[S]);
	_ ->
	    abc
    end.

%% test2(Port) in the shell works
%% spawn(fun() -> bug:test2(Port) end  (error closed)
%% spawn(fun() -> bug:test2(Port), receive after infinity -> true end end).



test2(Port) ->
    case gen_tcp:listen(Port,
			[binary,{packet,4},{reuseaddr,true},{active,true}]) of
	{ok, Listen} ->
	    spawn(fun() ->  par_connect(Listen) end);
	_ ->
	    abc
    end.

par_connect(Listen) ->
    X = gen_tcp:accept(Listen),
    io:format("X=:~p~n",[X]).

bug2(Port) ->
    spawn(fun() ->  test2(Port) end).

bug3(Port) ->
    spawn(fun() ->  
		  test2(Port),
		  receive
		      after
			  infinity ->
			      true
		      end
	  end).


