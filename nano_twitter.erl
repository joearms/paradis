-module(nano_twitter).
-compile(export_all).

%% in terminal one
%%   $ erl -noshell -sname twit -s nano_twitter start
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
    pong = net_adm:ping('twit@air').

tweet(Msg) ->
    rpc:cast('twit@air', erlang, send, [twit, Msg]).

print(X) ->
    io:format("~p~n",[X]).

