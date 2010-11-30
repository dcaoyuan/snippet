%% Author Anders Nygren
%% Based on Steve Vinoski's tbray16, see below.
%% Changes: 
%% - Use ets instead of dictionaries for collecting matches.
%% - Tweak heap size
%%
%% tbray16 -- another Erlang solution to Tim Bray's Wide Finder project
%% Author: Steve Vinoski (http://steve.vinoski.net/), 21 October 2007.
%% See <http://steve.vinoski.net/blog/2007/10/21/faster-wf-still/>.
%% 
-module(tbray_blockread).
-export([start/1, start/2, main/1]).
-export([scan_file/6, receive_accs/1]).
-compile([native]).

receive_matches(Tab) ->
    receive
        Match -> 
	    case catch ets:update_counter(Tab, Match, 1) of
                     N when is_integer(N) -> ok;
                     _ -> ets:insert(Tab, {Match, 1})
                 end,
                receive_matches(Tab)
            end.

split_and_find(Bin, Blksize, Pid, Tbl, Rcv, Tid) ->
    Size = Blksize - 1,
    case Bin of
        <<Front:Size/binary,$\n,Tail/binary>> ->
	    Me = self(),
	    spawn_opt(fun() -> wfbm4_ets:find(Front, Tbl, Rcv, Me, Tid) end,
		      [{min_heap_size, 10000}]),
            Tail;
        _ ->
            split_and_find(Bin, Size, Pid, Tbl, Rcv, Tid)
    end.

receive_accs(Workers) ->
    [receive done -> ok end || _ <- lists:seq(1, Workers)],
    ok.

scan_file(FD, Bin, Blksize, Tbl, Rcv, Tid) ->
    scan_file(FD, Bin, Blksize, Tbl, self(), Rcv, Tid, 0, Blksize).
scan_file(FD, Bin, Blksize, Tbl, Me, Rcv, Tid, Workers, Offset) ->
    Next = split_and_find(Bin, Blksize, Me, Tbl, Rcv, Tid),
    Offset1 = Offset - size(Next),    
    case file:pread(FD, Offset1, Blksize) of
	eof ->
	    Workers + 1;
	{ok, Block} ->
	    scan_file(FD, Block, Blksize, Tbl, Me, Rcv, Tid, 
		      Workers + 1, Offset1 + Blksize)
    end.


top_ten(Tab) ->
    lists:reverse(ets:foldl(fun (M,Acc) when length(Acc) <10 ->
				    lists:keysort(2, [M|Acc]);
				({_,X}=E, [{_,C}|Es]) when X>C-> 
				    lists:keysort(2, [E|Es]);
				(_E, Acc) ->
				    Acc
			    end, [], Tab)).

start(File, Blksize) ->
    Tid = ets:new(wftab, [set, public]),
    Rcv = spawn(fun() -> receive_matches(Tid) end),
    Tbl = wfbm4_ets:init(),
    {ok, F} = file:open(File, [read, binary]),
    {ok, Bin} = file:pread(F, 0, Blksize),
    Workers = scan_file(F, Bin, Blksize, Tbl, Rcv, Tid),
    file:close(F),
    receive_accs(Workers),
    L = top_ten(Tid),
    lists:map(fun({K,V}) -> io:format("~p: ~s~n", [V, K]) end, L).

start(File) ->
    Blksize = filelib:file_size(File) div (erlang:system_info(schedulers)*2),
    start(File, Blksize).

main([F, Blksize]) ->
    start(F, list_to_integer(Blksize)),
    halt();
main([F]) ->
    start(F),
    halt().

