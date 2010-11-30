-module(ets_test).

-export([start/0]).

-define(TAB, aaa).

start() ->
   T1 = ets:new(?TAB, [set]),
   T2 = ets:new(?TAB, [set]),
   ets:i().
