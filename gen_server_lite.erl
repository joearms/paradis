-module(gen_server_lite).
-export([start/2, loop/2, rpc/2]).


start(Mod, State) -> 
   register(Mod, spawn(gen_server_lite, loop, [Mod, State])).

loop(Mod, State, LastMessage) ->
   receive
      {From, Tag, Query} ->
	   case (catch Mod:handle(Query, State)) of
	       {'EXIT', Why} ->
		   error_logger:log_error({From, Query, State}),
		   exit(From, kill),
		   loop(Mod, State);
	       {Reply, State1} ->
		   LastMessag1 = add_messare(Query, LastMessage),
		   statistc_logger:log_this(..)
		   From ! {Tag, Reply},
		   loop(Mod, State1, LastMessage1)
	   end
       end.

rpc(Mod, Query) ->
    Tag = make_ref(),
    Mod ! {self(), Tag, Query},
    receive
       {Tag, Reply} ->
          Reply
    end.
