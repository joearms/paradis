-module(lists1).

-export([sum_and_double/1,
	 sum_and_double1/1,
	 sum_and_double2/1,
	 sum_and_double3/1,
	 upcase/1]).

sum_and_double(L) ->
    Sum = sum(L),
    Double = double(L),
    {Sum, Double}.

double([]) -> [];
double([H|T]) -> [2*H|double(T)].

sum([]) -> 0;
sum([H|T]) -> H + sum(T).

sum_and_double1(L) -> 
    sum_and_double_helper1(L, [], 0).

sum_and_double_helper1([H|T], L, N) ->
    sum_and_double_helper1(T, [2*H|L], N + H);
sum_and_double_helper1([], L, N) ->
    {N, L}.

sum_and_double2(L) -> 
    sum_and_double_helper2(L, [], 0).

sum_and_double_helper2([H|T], L, N) ->
    sum_and_double_helper2(T, [2*H|L], N + H);
sum_and_double_helper2([], L, N) ->
    {N, lists:reverse(L)}.

sum_and_double3(L) -> 
    sum_and_double3(L, [], 0).

sum_and_double3([H|T], L, N) ->
    sum_and_double3(T, [2*H|L], N + H);
sum_and_double3([], L, N) ->
    {N, lists:reverse(L)}.

upcase(X) when $a =< X andalso X =< $z ->
    X -$a + $A;
upcase(X) ->
    X.


    
