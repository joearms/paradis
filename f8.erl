-module(f8).
-compile(export_all).

tests() ->
    test_consult(),
    test_termio(),
    test_random_io(),
    test_dir_ops(),
    hooray.


test_consult() ->
    Term = [{hello,"joe"},
	    123],
    unconsult("abc", Term),
    Term = consult("abc"),
    yes.

consult(F) ->
    {ok, [L]} = file:consult(F),
    L.

unconsult(File, Term) ->
    {ok, S} = file:open(File, [write]),
    io:format(S, "~p.~n",[Term]),
    file:close(S).

%%----------------------------------------------------------------------
%% term_io

test_termio() ->
    Term = {a,term,123,"hello"},
    term_to_file("abc", Term),
    Term = file_to_term("abc").
 
term_to_file(File, X) ->
    file:write_file(File, term_to_binary(X)).

file_to_term(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_term(Bin).

%% test_random_io

test_random_io() ->
    file:write_file("abc",<<"0123456789">>),
    {ok, S} = file:open("abc", [read,raw,binary]),
    {ok,<<"0123">>} = file:pread(S, 0, 4),
    {ok, <<"678">>} = file:pread(S, 6, 3),
    {ok, <<"78">>} = file:pread(S, 7, 2),
    {ok, <<"789">>} = file:pread(S, 7, 3),
    {ok, <<"789">>} = file:pread(S, 7, 10),
    file:close(S).

test_dir_ops() ->
    {ok, L} = file:list_dir("."),
    [classify(I) || I <- L].

classify(F) ->
    case filelib:is_file(F) of
	true ->
	    file;
	false ->
	    case filelib:is_dir(F) of
		true ->
		    dir;
		false ->
		    unknown
	    end
    end.
