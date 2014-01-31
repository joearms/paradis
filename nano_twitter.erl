-module(nano_twitter).
-compile(export_all).

%% in terminal one
%%   $ erl -noshell -sname tweets -s nano_twitter start
%% in termianal two
%%   $ erl -sname anything
%%   > nano_twitter:connect().
%%   > nano_twitter:tweet(test).

start() ->
    register(twit, spawn(nano_twitter, watcher, [])).

watcher() ->
    receive
	Any ->
	    print({tweet, Any}),
	    watcher()
	after 5000 ->
		print(yawn),
		watcher()
	end.

connect() ->
    pong = net_adm:ping('twit@joe').

tweet(Msg) ->
    rpc:cast('twit@joe', erlang, send, [twit, Msg]).

print(X) ->
    io:format("~p~n",[X]).

