-module(rsa_test).

-compile(export_all).

test() ->
    rsa_key:make_sig("joe", 100),
    tests().

tests() ->
    test1(),
    test2(),
    test3(),
    test4(),
    io:format("horray~n"),
    init:stop().
 
test1() ->
    Msg = "The quick brown fox jumps of the lazy dog",
    Enc = encode("joe.pub", Msg),
    Msg = decode("joe.pri", Enc).

test2() ->
    Msg = "The quick brown fox jumps of the lazy dog",
    Enc = encode("joe.pri", Msg),
    Msg = decode("joe.pub", Enc).

test3() ->
    Data = "The quick brown fox jumps of the lazy dog",
    Certificate = sign("joe.pri", Data),
    valid = validate("joe.pub", Certificate, Data).

test4() ->
    Data = "The quick brown fox jumps of the lazy dog",
    Certificate = sign("joe.pub", Data),
    valid = validate("joe.pri", Certificate, Data).

validate(File, Certificate, Data) ->
    SHA = crypto:hash(sha,Data),
    I = lin:bin2int(SHA),
    {B, N} = read_key(File),
    case lin:pow(Certificate, B, N) of
	I -> valid;
	_ -> invalid
    end.   

sign(File, Data) ->
    SHA = crypto:hash(sha,Data),
    I = lin:bin2int(SHA),
    {A, N} = read_key(File),
    lin:pow(I, A, N).

encode(File, Str) ->
    I = lin:str2int(Str),
    {A, N} = read_key(File),
    lin:pow(I, A, N).

decode(File, I) ->
    {B, N} = read_key(File),
    I1 = lin:pow(I, B, N),
    lin:int2str(I1).

read_key(F) ->
    {ok, Bin} = file:read_file(F),
    binary_to_term(Bin).

