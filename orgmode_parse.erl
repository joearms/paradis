-module(orgmode_parse).
-compile(export_all).
-import(lists, [reverse/1]).

test() ->
    transform("f2-f3.org").

transform([X]) ->
    F = atom_to_list(X),
    io:format("Transforming:~p~n",[F]),
    {ok, Bin} = file:read_file(F),
    Blocks = parse(Bin),
    Blocks1 = [pass1(I) || I <- Blocks],
    %% elib2_misc:dump("blocks.tmp", Blocks1),
    {A0, Blocks2} = extract_header(Blocks1),
    io:format("A0=~p~n",[A0]),
    ModS = proplists:get_value("process", A0),
    Mod = list_to_atom(ModS),
    case (catch apply(Mod, process, [F, A0, Blocks2])) of
	{'EXIT', {undef, LL}} ->
	    %% elib2_misc:dump("error.tmp", hd(LL)),
	    HH = hd(LL),
	    io:format("Error Undefined Module:~p func:~p~n",
		      [element(1,HH), element(2,HH)]);
	Z ->
	    io:format("Z:~p~n",[Z])
    end.

extract_header([{p,"#+STARTUP:" ++ _}|T]) ->
    extract_header(T);
extract_header([{kv,A}|T]) ->
    {A, T};
extract_header(L) ->
    io:format("File has invalid header:~p~n",[hd(L)]).

parse(Bin) ->
    L = parse("\n" ++ binary_to_list(Bin), []),
    fixup(L).

fixup([{p,"\n" ++ X}|T]) -> [{p,X}|T];
fixup(X) -> X. 
    

parse([], L) ->
    reverse(L);
parse("\n*" ++ T, L) ->
    {Line1, T1} = get_line(T, []),
    {Line2, N} = count($*, Line1, 1),
    parse(T1, [{star, N, Line2}|L]);
parse("\n+" ++ T, L) ->
    {Line1, T1} = get_line(T, []),
    {Line2, N} = count($+, Line1, 1),
    parse(T1, [{plus, N, Line2}|L]);
parse("\n#+BEGIN" ++ T, L) ->
    {Tag, T1} = get_line(T, []),
    Stop = "\n#+END" ++ Tag,
    %% io:format("Stop:~p~n",[Stop]),
    {Body, T2} = get_body(T1, Stop, []),
    %% io:format("Body:~p~n",[Body]),
    parse(T2, [{block, Tag, Body}|L]);
parse(T, L) ->
    {Data, T1} = get_untagged(T, []),
    parse(T1, [{p,Data}|L]).

count(H, [H|T], N) -> count(H, T, N+1);
count(_, L, N)     -> {L, N}. 
    
get_line("\n" ++ _=T, L) -> {reverse(L), T};
get_line([H|T], L)       -> get_line(T, [H|L]); 
get_line([], L)          -> {reverse(L), []}. 

get_body([H|T] = X, Stop, L) -> 
    case is_stop(X, Stop) of
	{yes, X1} -> {reverse(L), X1};
	no -> get_body(T, Stop, [H|L])
    end;
get_body([], Stop, L) ->
    io:format("Note terminated:~p Clue:~p~n",[Stop,lists:reverse(L)]),
    exit(eBadInput).

is_stop(X, []) ->
    {Line, X1} = get_line(X, []),
    case is_blank(Line) of
	true ->
	    void;
	false ->
	    io:format("*** Warning non bank after stop~p:~n",[Line])
    end,
    {yes, X1};
is_stop([H|T], [H|T1]) ->
    is_stop(T, T1);
is_stop(_, _) ->
    no.

get_untagged("\n*" ++ _ = X, L)       -> {reverse(L), X};
get_untagged("\n+" ++ _ = X, L)       -> {reverse(L), X};
get_untagged("\n#+BEGIN" ++ _ = X, L) -> {reverse(L), X};
get_untagged([H|T], L)                -> get_untagged(T, [H|L]);
get_untagged([], L)                   -> {reverse(L), []}.

is_blank([$\s|T]) -> is_blank(T);
is_blank([])      -> true;
is_blank(_)       -> false.

pass1({block,"_kv",Str}) -> parse_kv(Str);
pass1(X)                 -> X.

parse_kv(Str) ->
    Lines = string:tokens(Str,"\n"),
    {kv, [kv(I) || I<- Lines]}.

kv(Str) ->
    case string:tokens(Str, ":") of
	[A, B] ->
	    {elib2_misc:trim(A), elib2_misc:trim(B)};
	_ ->
	    io:format("bad KV:~p~n",[Str]),
	    exit(badKV)
    end.



	    
