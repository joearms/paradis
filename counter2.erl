-module(counter2).
-export([start/0, init/1, tick/1, read/0, clear/0, handle_call/3]).

%% the real gen_server

start() -> gen_server:start_link({local,counter2}, counter2, 0, []).

init(N) -> {ok, N}.

tick(N)  -> gen_server:call(counter2, {tick, N}).
read()   -> gen_server:call(counter2, read).
clear()  -> gen_server:call(counter2, clear).

handle_call({tick,N}, _From, M)   -> {reply, ack, M+N};
handle_call(read, _From, N)       -> {reply, N, N};
handle_call(clear,_From, _State)  -> {reply, ok, 0}.
