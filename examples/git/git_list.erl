-module(git_list).

-compile(export_all).

test() ->
    Dir = "../../.git/objects",
    {ok, L} = file:list_dir(Dir),
    L1 = L -- ["info","pack"],
    lists:flatten([do(Dir ++ "/" ++ I) || I <- L1]).

do(D) ->
    {ok, L} = file:list_dir(D),
    [obj(D ++ "/" ++ I)  || I <- L].

obj(X) ->
    io:format("Reading~p~n",[X]),
    {ok, B} = file:read_file(X),
    B1 = zlib:uncompress(B),
    CC = hex:bin_to_hexstr(crypto:hash(sha,B1)),
    TypeSizeData = unpack_object(B1),
    {X, parse_git_object(TypeSizeData)}.

unpack_object(B) ->
    [Type,Rest] = binary:split(B, <<32>>),
    [Len,Data] = binary:split(Rest,<<0>>),
    {Type, Len, Data}.
    
%% <bof>treespace>DDDDD<zero>

%% <bof>tag<space>135<zero>object 8bbad744fe9597cc130e8ef7cfd201f36d9cba76
%% type commit
%% tag v1.2
%% tagger Scott Chacon <schacon@gmail.com> 1258278905 +0100

%% <bof>commit<space>225<zero>tree 47e3fc6cde2a1552c28ab97361ff54bbae3c6f60
%% parent 25daa907ccb6feb267bfec70a130d5fe13e48a79
%% author Scott Chacon <schacon@gmail.com> 1258278898 +0100
%% committer Scott Chacon <schacon@gmail.com> 1258278898 +0100
%% new tag

parse_git_object({<<"tree">>, _, B}) -> 
    {tree, parse_git_tree(B)};
parse_git_object({<<"blob">>, Len, B}) ->
    {blob,{length,Len},B};
parse_git_object({<<"commit">>, _Len, B}) ->
    {commit, parse_git_commit(B)};
parse_git_object(X) ->
    {not_yet_parsed, X}.

%% <bof>blob<space>NNN<zero>....

parse_git_tree(<<>>) ->
    [];
parse_git_tree(B) ->
    [Mode,B1] = binary:split(B, <<32>>),
    [FileName, <<Sha1:20/binary,Rest1/binary>>] = binary:split(B1, <<0>>),
    Hex = hex:bin_to_hexstr(Sha1),
    [{object_type(Mode),FileName,Hex}|parse_git_tree(Rest1)].
    
object_type(<<"100644">>) -> blob;
object_type(<<"040000">>) -> tree;
object_type(X)            -> X.
 
parse_git_commit(B) ->
    Z = binary:split(B,<<"\n">>,[global]),
    parse_commit1(Z).

parse_commit1([]) ->
    [];
parse_commit1(L) ->
    {Entry, Rest} = parse_commit2(L, #{}),
    [Entry|parse_commit1(Rest)].

parse_commit2([<<"tree ",X/binary>>|T], E) ->
    parse_commit2(T, E#{tree => X});
parse_commit2([<<"parent ",X/binary>>|T], #{parents := P} = E) ->
    parse_commit2(T, E#{parents := [X|P]});
parse_commit2([<<"parent ",X/binary>>|T], E) ->
    parse_commit2(T, E#{parents => [X]});
parse_commit2([<<"author ",X/binary>>|T], E) ->
    parse_commit2(T, E#{author => X});
parse_commit2([<<"committer ",X/binary>>|T], E) ->
    parse_commit2(T, E#{committer => X});
parse_commit2([<<>>|T], E) ->
    {Msg, T1} = collect_message(T, []),
    E1 = E#{msg=>Msg},
    {E1, T1}.

collect_message([], L)         -> {lists:reverse(L), []};
collect_message([<<>>|T]=T, L) -> {lists:reverse(L), T};
collect_message([H|T], L)      -> collect_message(T, [H|L]).






