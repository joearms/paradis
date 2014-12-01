-module(git_parse_index).
%% git ls-files --stage
%% git ls-files -v --debug > debug
-compile(export_all).

test() ->
    parse(os:getenv("HOME") ++ "/Dropbox/experiments/paradis/.git/index").

parse(F) ->
    {ok, B} = file:read_file(F),
    L = parse_bin(B),
    elib2_misc:dump("tmp", L).

parse_bin(<<"DIRC",Vsn:32,Nentries:32,B/binary>>) ->
    io:format("Vsn:~p Nentries:~p~n",[Vsn, Nentries]),
    L = lists:reverse(parse_entries(B, Nentries, Vsn, [])),
    {Vsn,Nentries,L}.


parse_entries(_, 0, _Vsn, L) ->
    L;
%% parse_entries(B, Vsn, L) when length(L) > 8 ->
%%     L;
parse_entries(B, N, Vsn, L) ->
    {E, B1} = parse_entry(Vsn, B),
    #{name := Name, sha1 := SHAB, mode := M} = E,
    CC = hex:bin_to_hexstr(SHAB),
    io:format("~s ~s ~s~n",[M, CC, Name]),
    parse_entries(B1, N-1, Vsn, [E|L]).

parse_entry(2 = Vsn, 
	    << CtimeSecs:32,
	       CtimeNano:32,
	       MtimeSecs:32,
	       MtimeNano:32,
	       Dev:32, 
	       Ino:32,
	       %% ObjType:4,_:3,Perm:9,_:16, %% padded with 16 bits
	       Mode:32,
	       Uid:32, 
	       Gid:32,
	       Size:32,
	       Sha1:20/binary,
	       %% Flags
	       Flags:4, NameLen:12,
	       Rest/binary>>) ->
    %% First = first(entry, B0, 150),
    {Name, Npad, Rest1} = get_name(Rest),
    Entry = #{%%a => First,
      vsn => Vsn,
      ctimeSecs => CtimeSecs,
      ctimeNano => CtimeNano,
      mtimeSecs => MtimeSecs,
      mtimeNamo => MtimeNano,
      mode => integer_to_list(Mode,8), %% {ObjType, Perm},
      pad => {Npad, length(Name), length(Name) rem 8},
      uid => Uid,
      flags => {Flags,NameLen},
      ino => Ino,
      gid => Gid,
      dev => Dev,
      size => Size,
      sha1 => Sha1,
      name => Name},
    {Entry, Rest1}.

first(Tag, B, Len) ->
    {B1,_} = split_binary(B,Len),
    {Tag,B1}.

get_name(B) ->
    %% show(get_name, B, 5),
    get_name(B, 0, []).

get_name(<<0,B/binary>>, N, L) ->
    Name = lists:reverse(L),
    N = length(Name),
    {Npad, B1} = skip(N, B),
    {Name, Npad, B1};
get_name(<<H,B/binary>>, N, L) ->
    get_name(B,N+1,[H|L]).

skip(N, B) ->
    Nchars = chars_to_skip(N),
    %% io:format(" ~p chars skipping ~p~n",[N,Nchars]),
    {_, B2} = split_binary(B, Nchars),
    {Nchars, B2}.

chars_to_skip(N) ->
    case 9 - (N rem 8) of
	8 ->
	    0;
	K ->
	    K
    end.
    


		



