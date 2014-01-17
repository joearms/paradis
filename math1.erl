-module(math1).
-export([double/1, sum/1]).

double([]) -> [];
double([H|T]) -> [2*H|double(T)].



sum(L) -> sum_helper(L, 0).

sum_helper([], N) -> N;
sum_helper([H|T], N) ->
    N1 = N + H,
    sum_helper(T, N1).

%% or sum_helper([H|T], N) -> sum_helper(T, N+H)

    
