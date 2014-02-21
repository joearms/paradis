-module(router).
-compile(export_all).

start(Name) ->
    register(Name, spawn(fun() -> 
				 put(name, Name),
				 router() 
			 end)).

router() ->
    receive
	{do,{add_route,Tag,Pid},Epid} ->
	    case get(Tag) of
		undefined -> put(Tag, Pid);
		_         -> Epid ! {eBadTag, get(name), Tag}
	    end;
	{do, {route, Tag, Msg}, Epid} ->
	    case get(Tag) of
		undefined -> Epid ! {ebadRoute, get(name), Tag};
		Pid       -> Pid ! Msg
	    end;
	{do, {remove_route, Tag}} ->
	    erase(Tag)
    end,
    router().

