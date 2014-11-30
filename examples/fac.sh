#!/usr/bin/env escript

main([X]) ->
    N = list_to_integer(X),
    io:format("factorial(~p)=~p~n",[N, fac(N)]),
    init:stop().

fac(0) ->
    1;
fac(N) when N > 0 ->
    N * fac(N-1).




    
