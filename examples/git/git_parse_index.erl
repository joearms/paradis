-module(git_parse_index).

-compile(export_all).

test() ->
    parse(os:getenv("HOME") ++ "/Dropbox/experiments/paradis/.git/index").

parse(F) ->
    {ok, B} = file:read_file(F),
    parse_bin(B),
    a.

parse_bin(<<"DIRC",Vsn:32,Nentries:32,B/binary>>) ->
    io:format("Vsn:~p Nentries:~p~n",[Vsn, Nentries]),
    L = parse_entries(B, Vsn, []),
    {Vsn,Nentries,L}.

bug() ->
    parse_entry(2,

<<83,39,65,225,0,0,0,0,1,0,0,2,0,104,66,92,0,0,129,164,0,0,1,
                   245,0,0,0,20,0,0,0,235,189,28,17,111,207,232,157,72,105,34,
                   230,1,169,105,164,219,172,131,193,160,0,46,99,108,105,101,
                   110,116,95,115,101,114,118,101,114,95,108,101,99,116,117,
                   114,101,47,114,115,97,95,112,117,114,101,95,101,114,108,97,
                   110,103,47,77,97,107,101,102,105,108,101,0,0,0,0,84,110,32,
                   210,0,0,0,0,83,39,18,184,0,0,0,0,1,0,0,2,0,104,66,14,0,0,
                   129,164,0,0,1,245,0,0,0,20,0,0,7,121,222,247,39,135,109,31>>
		    ).



parse_entries(<<>>, _Vsn, L) ->
    L;
parse_entries(B, Vsn, L) when length(L) > 8 ->
    L;
parse_entries(B, Vsn, L) ->
    io:format("getitng:~p~n",[length(L)]),
    {E, B1} = parse_entry(Vsn, B),
    io:format("recovered:~p~n",[E]),
    parse_entries(B1, Vsn, [E|L]).

parse_entry(2 = Vsn, <<CtimeSecs:32,
		 CtimeNano:32,
		 MtimeSecs:32,
		 MtimeNano:32,
		 Dev:32, 
		 Ino:32,
		 Obj:4,_:3,Perm:9,_:16,
		 Uid:32, Gid:32,
		 Size:32,
		 Sha1:20/binary,
		 %% Flags
		 Flags:4,
		 NameLen:12,
		 Rest/binary>> = B0) ->
    show(entry, B0, 150),
    {Name, Rest1} = get_name(Rest),
    Entry = #{vsn => Vsn,
	      ctimeSecs => CtimeSecs,
	      ctimeNano => CtimeNano,
	      mtimeSecs => MtimeSecs,
	      mtimeNamo => MtimeNano,
	      perm => Perm,
	      uid => Uid,
	      flags => {Flags,NameLen},
	      ino => Ino,
	      gid => Gid,
	      dev => Dev,
	      size => Size,
	      sha1 => Sha1,
	      name => Name},
    {Entry, Rest1}.

show(Tag, B, Len) ->
    {B1,_} = split_binary(B,Len),
    io:format("Looking at:~p ~p~n",[Tag, B1]).


get_name(B) ->
    show(get_name, B, 5),
    get_name(B, 0, []).

get_name(<<0,B/binary>>, N, L) ->
    Name = lists:reverse(L),
    B1 = skip(N, B),
    {Name, B1};
get_name(<<H,B/binary>>, N, L) ->
    get_name(B,N+1,[H|L]).

skip(N, B) ->
    Nchars = chars_to_skip(N),
    io:format(" ~p chars skipping ~p~n",[N,Nchars]),
    {_, B2} = split_binary(B, Nchars),
    B2.

chars_to_skip(N) ->
    9 - (N rem 8).

    


		



