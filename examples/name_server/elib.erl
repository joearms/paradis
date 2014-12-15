-module(elib).
-export([my_ip/0]).

my_ip() ->
    {ok, L} = inet:getifaddrs(),
    {value,{_,L1}} = lists:keysearch("en0", 1, L),
    get_ip4(L1).

get_ip4([{addr,{A,B,C,D}}|_]) -> {A,B,C,D};
get_ip4([_|T])                -> get_ip4(T);
get_ip4([])                   -> [].
