-module(math0).
-vsn(1312379126381623).

-import(math, [pi/0]).

%% a comment

%% -export([area/1]).

-compile(export_all).


double([]) -> [];
double([H|T]) -> [2*H|double(T)]. 

triple([]) -> [];
triple([H|T]) -> [3*H|triple(T)]. 

double_and_triple(L) ->
    {double(L), triple(L)}.

double_and_triple1(L) ->
    double_and_triple1(L, [], []).

double_and_triple1([H|T], Doubles, Triples) ->
    double_and_triple1(T, [2*H|Doubles], [3*H|Triples]);
double_and_triple1([], D, T) ->
    {lists:reverse(D), lists:reverse(T)}.

reverse(L) -> reverse(L, []).

reverse([H|T], L) -> reverse(T, [H|L]);
reverse([], L)    -> L.

map(F, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].

map1(F, L) -> [F(I) || I <- L].

 
    

for(Max, Max, F) -> [F(Max)];
for(I, Max, F)  when I =< Max -> [F(I) | for(I+1, Max, F)].


area2([square,X]) -> X*X;
area2([square,width,X,height,Y]) -> X*Y.
    

area({square,X})      -> X*X;
area({rectangle,X,Y}) -> X*X+3;
area({circle,R}) -> math:pi() * R * R.

weekend(saturday) -> true;
weekend(sunday) -> true; 
weekend(X) -> 
    case lists:member(X, [monday,tuesday,wednesday,thursday,friday]) of
	true ->
	    false;
	false ->
	    exit(bother)
    end.
    

area1(Arg) ->
    case Arg of
	{rectangle, X,Y} ->
	    X*Y;
	{square,X} ->
	    X*X;
	{circle,R} ->
	    pi()*R*R
    end.

fib(N) when N < 0 ->
    exit(noWay);
fib(N) when N < 2->
    1;
fib(N) ->
    fib(N-1) + fib(N-2).

test() ->
    100 = area({square,10}),
    120 = area({rectangle,10,12}),
    horray.

