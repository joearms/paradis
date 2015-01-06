%% From: https://github.com/rnewson/shamir/tree/master/src/galois.erl

%% Copyright 2011 Robert Newson
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(galois).

-export([generate/1, add/3, subtract/3, multiply/3, divide/3]).
-define(nw(W), (1 bsl W)).

-record(galois, {
    w,
    gflog,
    gfilog
}).

generate(W) ->
    generate(W, 1, 0).

generate(W, B, Log) ->
    generate(W, B, Log, dict:new(), dict:new()).

generate(W, _B, Log, Gflog, Gfilog) when Log =:= ?nw(W) -1 ->
    #galois{w=W, gflog=Gflog, gfilog=Gfilog};
generate(W, B, Log, Gflog, Gfilog) ->
    Gflog1 = dict:store(B, Log, Gflog),
    Gfilog1 = dict:store(Log, B, Gfilog),
    B1 = B bsl 1,
    B2 = if
        B1 band ?nw(W) > 0 ->
            B1 bxor prim_poly(W);
        true ->
            B1
    end,
    generate(W, B2, Log + 1, Gflog1, Gfilog1).

multiply(#galois{}, 0, _) ->
    0;
multiply(#galois{}, _, 0) ->
    0;
multiply(#galois{w=W, gflog=Gflog, gfilog=Gfilog}, A, B) ->
    case dict:fetch(A, Gflog) + dict:fetch(B, Gflog) of
        SumLog when SumLog >= ?nw(W) - 1 ->
            dict:fetch(SumLog - (?nw(W) - 1), Gfilog);
        SumLog ->
            dict:fetch(SumLog, Gfilog)
    end.

divide(#galois{}, 0, _) ->
    0;
divide(#galois{}, _, 0) ->
    throw(division_by_zero);
divide(#galois{w=W, gflog=Gflog, gfilog=Gfilog}, A, B) ->
    case dict:fetch(A, Gflog) - dict:fetch(B, Gflog) of
        DiffLog when DiffLog < 0 ->
            dict:fetch(DiffLog + (?nw(W) - 1), Gfilog);
        DiffLog ->
            dict:fetch(DiffLog, Gfilog)
    end.

add(#galois{}, A, B) ->
    A bxor B.

subtract(#galois{}, A, B) ->
    A bxor B.

prim_poly(4) ->
    8#23;
prim_poly(8) ->
    8#435;
prim_poly(16) ->
    8#210013.


