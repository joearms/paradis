-module(week4_problems).
-export([test_easy/0, test_hard/0, dump/2]).

test_easy() ->
    M = week4_solutions,
    120 = M:factorial(5),
    L = [a,b,c,d,e,f],
    [b,c,d,e,f,a] = M:rotate(1,L),
    [f,a,b,c,d,e] = M:rotate(-1,L),
    [d,e,f,a,b,c] = M:rotate(3, L),
    [e,f,a,b,c,d] = M:rotate(-2, L),
    [b,c,d,e,f,a] = M:rotate(7,L),
    [b,c,d,e,f,a] = M:rotate(100000000000000000*6+1,L),
    "<b>BB</b>"   = M:expand_markup("**BB**"),
    "<b>BB</b><i>II</i>"   = M:expand_markup("**BB**__II__"),
    "<b>BB<i>II</i>CC</b>" = M:expand_markup("**BB__II__CC**"),
    horray.

test_hard() ->
    "<b>BB<i>II</i></b><i>CC</i>" = 
	week4_solutions:expand_markup("**BB__II**CC__"),
    test_count_atoms(),
    whoopy.


test_count_atoms() ->
    X = epp:parse_file("week4_problems.erl",[],[]),
    dump("debug", X),
    8 = week4_solutions:count_atoms(a, X).

dump(File, X) ->
    {ok, S} = file:open(File, [write]),
    io:format(S, "~p~n", [X]),
    file:close(S),
    io:format("written:~p~n",[File]).
