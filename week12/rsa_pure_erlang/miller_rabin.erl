-module(miller_rabin).
-compile(export_all).
-export([test/0, test1/0, test2/0,test3/0,test4/0,
	 is_probably_prime/1, make_prime/1]).

%% def miller_rabin_prime?(n,k)
%%   return true if n == 2
%%   return false if n < 2 or n % 2 == 0
%%   d = n - 1
%%   s = 0
%%   while d % 2 == 0
%%     d /= 2
%%     s += 1
%%   end
%%   k.times do
%%     a = 2 + rand(n-4)
%%     x = (a**d) % n
%%     next if x == 1 or x == n-1
%%     for r in (1 .. s-1)
%%       x = (x**2) % n
%%       return false if x == 1
%%       break if x == n-1
%%     end
%%     return false if x != n-1
%%   end
%%   true  # probably
%% end
 
%% with maximal help from crypto

test() ->
    make_prime(1024).

test1() ->
    [I || I <- lists:seq(1, 1000), is_probably_prime(I)].

test2() ->
    N = 31987937737479355332620068643713101490952335301,
    A = lin:pow(2,N-1,N),
    {A,is_probably_prime(N)}.

test3() ->
    true = is_probably_prime(643808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153),
    false = is_probably_prime(743808006803554439230129854961492699151386107534013432918073439524138264842370630061369715394739134090922937332590384720397133335969549256322620979036686633213903952966175107096769180017646161851573147596390153),
    yes.

test4() ->
    {_,A} = make_prime(1024),
    {_,B} = make_prime(1024),
    false = is_probably_prime(A*B),
    {A*B,is_composite}.

-spec make_prime(LengthInBits::integer()) ->
			{NumberOfTrials::integer(),
			 Prime::integer()}.

make_prime(Len) ->
    make_prime(Len, 0).

make_prime(Len, K) ->
    N = crypto:strong_rand_mpint(Len,1,1),
    M = pint_to_int(N, 0),
    case is_probably_prime(M) of
	false -> make_prime(Len, K+1);
	true  -> {K,M}
    end.

pint_to_int(<<>>,N) -> N;
pint_to_int(<<X:8,B/binary>>,N) ->
    pint_to_int(B, N*256+X).

-spec is_probably_prime(integer()) -> boolean().

%% runs the rabbin miller rabit test 40 times
%% to test for a prime number

is_probably_prime(2) -> true;
is_probably_prime(N) when N < 2 -> false;
is_probably_prime(N) ->
    case is_even(N) of
	true -> false;
	false ->
	    {S, D} = sd(N-1, 0),
	    loop(40, N, S, D)
    end.

loop(0,_,_,_) ->
    true;
loop(K, N, S, D) ->
    A = 2 + crypto:rand_uniform(1,N-4),
    X = lin:pow(A,D,N),
    N1 = N - 1,
    case X of
	1  -> loop(K-1, N, S, D);
	N1 -> loop(K-1, N,S,D);
	_  -> inner(S-1, K, N, X, S, D)
    end.

inner(0, K, N, X, S, D) ->
    case N-1 of
	X -> loop(K-1, N, S, D);
	_ -> false
    end;
inner(R, K, N, X, S, D) ->
    X1 = lin:pow(X,2,N),
    N1 = N - 1,
    case X1 of
	1  -> false;
	N1 -> inner(0, K, N, X1, S, D);
	_  -> inner(R-1,K,N,X1,S,D)
    end.
		   
sd(K, N) ->
    case is_even(K) of
	true  -> sd(K div 2, N+1);
	false -> {N, K}
    end.
    
is_even(K) ->
    K band 1 == 0.

%% greatest common devisor

gcd(A, B) when A < B -> gcd(B, A);
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).
