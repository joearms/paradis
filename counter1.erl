-module(counter1).
-compile(export_all).

start() -> gen_server_lite:start(counter1, 0).

tick(N)  -> gen_server_lite:rpc(counter1, {tick, N}).
read()   -> gen_server_lite:rpc(counter1, read).
clear()  -> gen_server_lite:rpc(counter1, clear).

handle({tick,N}, M) -> {ack, M+N};
handle(read, N)     -> {N, N};
handle(clear,_)     -> {ok, 0}.
