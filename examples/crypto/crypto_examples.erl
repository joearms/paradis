-module(crypto_examples).
-compile(export_all).

digest1() ->
    %% sha means "sha1"
    crypto:hash(sha, "hello world").

digest2() ->
    S0 = crypto:hash_init(sha),
    S1 = crypto:hash_update(S0, "hello "),
    S2 = crypto:hash_update(S1, "to"),

    crypto:hash_final(S2).

test1() ->
    K1 = crypto:stream_init(rc4, "secret password"),
    {K2, C1} = crypto:stream_encrypt(K1, "hello "),
    {K3, C2} = crypto:stream_encrypt(K2, "world"),
    %% ...
    {S1, M1} = crypto:stream_decrypt(K1, C1),
    {S2, M2} = crypto:stream_decrypt(S1, C2),
    {M1, M2}.


    



bin2hex(B) ->
    L = binary_to_list(B),
    LH0 = lists:map(fun(X)->erlang:integer_to_list(X,16) end, L),
    LH = lists:map(fun([X,Y])->[X,Y];([X])->[$0,X] end, LH0), % add zeros
    lists:flatten(LH).
