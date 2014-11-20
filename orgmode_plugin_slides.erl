-module(orgmode_plugin_slides).

-export([process/3]).
-import(orgmode_parse, [parse_kv/1]).
-import(lists, [reverse/1]).

process(File, A0, Blocks) ->
    Root = filename:rootname(File),
    Blocks1 = [pass1(I) || I <- Blocks],
    L = render(Blocks1, [], []),
    %% elib2_misc:dump("parsed.tmp", L),
    A = [{"slides", L}|A0],
    Out = "lecture_" ++ Root ++ ".tex",
    elib2_misc:expand_file_template("slides.template", A, Out),
    io:format("Created:~s~n",[Out]).

pass1({block,"_kv",Str}) ->
    parse_kv(Str);
pass1({block,"_image",Str}) ->
    {kv, A} = parse_kv(Str),
    Ht = proplists:get_value("height", A),
    Image = proplists:get_value("image", A),
    Title = proplists:get_value("title", A),
    L = ["\\begin{center}\n",
	 "\\includegraphics[height=",Ht,"]{",Image,"}\\\\\n",
	 Title,"\n",
	 "\\end{center}\n"],
    {latex, L};
pass1({block,"_shell",Str}) ->
    L = ["\\begin{shell}",
	 Str,"\n",
	 "\\end{shell}\n"],
    {latex, L};
pass1({block,"_erlang",Str}) ->
    Str1 = color_erlang:string(Str),
    L = ["\\begin{erl}",
	 Str1,"\n",
	 "\\end{erl}\n"],
    {latex, L};
pass1({block,"_sublist",Str}) ->
    K = string:tokens(Str,"\n"),
    L = ["\\begin{itemize}\n",
	 [["\\item ",I,"\n"] || I <- K],
	 "\\end{itemize}\n"],
    {latex, L};
pass1({block, Type, _} = B) ->
    io:format("************~n*********skipping block:~p~n",[Type]),
    B;
pass1(X) ->
    X.



render([{star,1,Title}|T], S, L) ->
    %% new page
    {S1, L1} = close_slide(S, L),
    render(T, S1, [["\\section{",Title,"}\n"]|L1]);
render([{star,2,Line}|T], S, L) ->
    {S1, L1} = open_enum(S, L),
    render(T, S1, [["\\item ",Line,"\n"]|L1]);
render([{p,Str}|T], S, L) ->
    render(T, S, [["\n",Str,"\n"]|L]);
render([{block,_,Str}|T], S, L) ->
    render(T, S, [["\\begin{verbatim}\n",Str,"\n\\end{verbatim}\n"]|L]);
render([{latex,Str}|T], S, L) ->
    render(T, S, [Str|L]);
render([], S, L) ->
    {_S1, L1} = close_slide(S, L),
    reverse(L1).
    
close_slide([list|S], L) -> close_slide(S, ["\\end{itemize}\n"|L]);
close_slide(S, L)        -> {S, L}.

open_enum([list|_]=S, L) -> {S, L};
open_enum(S, L)          -> {[list|S], ["\\begin{itemize}\n"|L]}.
