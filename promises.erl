-module(promises).
-compile(export_all).

test1() -> fib(40).

test2() -> promise(fun() -> fib(40) end).

fib(0) -> 1;
fib(1) -> 1; 
fib(N) -> fib(N-1) + fib(N-2).

promise(F) ->
    Tag = make_ref(),
    S = self(),
    spawn(fun() -> S ! {Tag, F()} end),
    Tag.

query(Tag) ->
    receive
	{Tag, Reply} ->
	    {finished, Reply}
    after 0 ->
	    not_finished
    end.

yield(Tag) ->
    receive
	{Tag, Reply} ->
	    Reply
    end.
