-module(math2).
-export([test/0, area/1]). 

test() ->
    144 = area({square,12}),
    200 = area({rectangle,10,20}),
    hooray.

area({square,X}) -> X*X;
area({rectangle,X,Y}) -> X*Y.
