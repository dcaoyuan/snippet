%%% The Computer Language Benchmarks Game
%%% http://shootout.alioth.debian.org/
%%% Contributed by Jiri Isa

-module(threadring).
%-compile([native]).
-export([main/0, ring/2, main_fib/1]).

-define(N_TOKENS, 10000).
-define(N_PROCS, 1000).

-define(N_FIB, 10000).

% time erl -smp -noshell -run threadring main
main() ->
   start(?N_TOKENS).

start(Token) ->
   H = lists:foldl(fun(Id, Pid) ->
                         spawn(threadring, ring, [Id, Pid])
                   end, self(), lists:seq(?N_PROCS, 2, -1)),
   H ! Token,
   ring(1, H).

ring(Id, Pid) ->
   receive
      1 ->
         io:fwrite("~b~n", [Id]),
         io:fwrite("~b~n", [Id]),
         erlang:halt();
      Token ->
         Pid ! Token - 1,
         io:fwrite("~b~n", [Token]),
         fib(?N_FIB),
         ring(Id, Pid)
   end.

main_fib(Arg) ->
   erlang:statistics(wall_clock),
   case Arg of
      [A1] ->
         F = list_to_integer(A1),
         fib_loop(1000, F);
      [A1, A2] ->
         F = list_to_integer(A1),
         N = list_to_integer(A2),
         fib_loop(N, F)
   end,
   {_, T1} = erlang:statistics(wall_clock),
   io:format("fib time: ~ps~n", [T1 / 1000]).
%erlang:halt().

fib_loop(0, _) -> done;
fib_loop(N, F) ->
   fib(F),
   fib_loop(N - 1, F).


fib(N) -> fib(N, 1, 0).
fib(0, _, Res) -> Res;
fib(N, Next, Res) -> fib(N - 1, Next + Res, Next).
