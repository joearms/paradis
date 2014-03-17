-module(maps).
-export([fib/1, pmap/2, smap/2]).

fib(1) ->
    1;
fib(2) ->
    1;
fib(N) ->
    fib(N-1) + fib(N-2).



smap(F, L) -> [F(I) || I <- L].

pmap(F, L) ->
    S = self(),
    Pids = [spawn(fun() ->
			  S ! {self(),catch F(I)}
		  end) || I <- L],
    gather_replies(Pids).

gather_replies([Pid|T]) ->
    receive
	{Pid, Val} -> [Val|gather_replies(T)]
    end;
gather_replies([]) ->
    [].
