-module(f6).
-compile(export_all).

time(N) ->
    {Time, _} = timer:tc(f6, time_test, [N]),
    Tsec = Time / 1000000,
    {spawned, trunc(N / Tsec), 'processes/sec'}.

time_test(0) ->
    true;
time_test(N) ->
    spawn(fun() -> true end),
    time_test(N-1).

start1() ->
    Pid = spawn(f6, loop1, []).

loop1() ->
    receive
	{square, X} -> 
	    print(X*X),
	    loop1()
    end.
	

start2() ->
    Pid = spawn(f6, loop2, []).

loop2() ->
    receive
	{From, {square, X}} -> 
	    From ! X*X,
	    loop2()
    end.

start3() ->
    Pid = spawn(f6, loop3, []).

loop3() ->
    receive
	{From, {square, X}} -> 
	    From ! {self(), X*X},
	    loop3()
    end.
	
area_square(Pid, X) ->
    Pid ! {self(), {square, X}},
    receive
	{Pid, Area} ->
	    Area
    end.

print(X) ->	  
    io:format("~p~n",[X]).

start_and_watch() ->
    Pid = spawn(f6, loop1, []),
    spawn(f6, watch, [Pid]),
    Pid.

watch(Pid) ->
    link(Pid),
    process_flag(trap_exit, true),
    print({watching, Pid}),
    receive
	{'EXIT', Pid, Why} ->
	    print({ohDear,Pid,crashed,because,Why})
    end.


    
