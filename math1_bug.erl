-module(math1_bug).

-export([add/2, bug/1, fac/1]).

add(X, Y) -> X+Y.

bug(X) ->
    add("hello", X).

fac(0) ->
    1;
fac(N) ->
    N * fac(N-1).

    
