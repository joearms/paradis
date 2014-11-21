-module(gen_server_lite).
-export([start/2, loop/3, rpc/2]).


start(Mod, State) -> 
   register(Mod, spawn(gen_server_lite, loop, [Mod, State, []])).

loop(Mod, State, LastMessages) ->
   receive
      {From, Tag, Query} ->
	   case (catch Mod:handle(Query, State)) of
	       {'EXIT', Why} ->
		   error_logger:log_error({From, Query, State, Why}),
		   exit(From, kill),
		   loop(Mod, State, LastMessages);
	       {Reply, State1} ->
		   LastMessages1 = add_message(Query, LastMessages),
		   statistics_logger:log_this("..."),
		   From ! {Tag, Reply},
		   loop(Mod, State1, LastMessages1)
	   end
       end.

rpc(Mod, Query) ->
    Tag = make_ref(),
    Mod ! {self(), Tag, Query},
    receive
       {Tag, Reply} ->
          Reply
    end.

add_message(Q, M) ->
    [M|Q].
