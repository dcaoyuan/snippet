% tbray16 -- another Erlang solution to Tim Bray's Wide Finder project
% Author: Steve Vinoski (http://steve.vinoski.net/), 21 October 2007.
% See <http://steve.vinoski.net/blog/2007/10/21/faster-wf-still/>.
-module(tbray16_tuple).
-export([start/1, start/2, main/1]).
-export([sort/1]).
-import(wfbm4).
-compile([native]).

receive_matches(Pid, Dict) ->
    receive
        done -> Pid ! Dict;
        Match -> receive_matches(Pid, dict:update_counter(Match, 1, Dict))
    end.

split_and_find(Bin, Blksize, Pid, Tbl) ->
    Size = Blksize - 1,
    case Bin of
        <<Front:Size/binary, $\n, Tail/binary>> ->
            Rcv = spawn_opt(fun() -> receive_matches(Pid, dict:new()) end,
			   [{min_heap_size,500}]),
            spawn_opt(fun() -> wfbm4_tuple:find(Front, Tbl, Rcv) end,
		      [{min_heap_size, 4000}
%%%		       ,{fullsweep_after, 10}
		      ]),
            {Rcv, Tail};
        _ ->
            split_and_find(Bin, Size, Pid, Tbl)
    end.

receive_tbls(L, Start_table) ->
    lists:foldl(fun(_, Table2) ->
                        receive Table1 ->
                                dict:merge(fun(_,V1,V2) -> V1 + V2 end, Table1, Table2)
                        end
                end, Start_table, L).
receive_tbls(L) ->
    receive_tbls(L, dict:new()).

scan_file(<<>>, _, _, _, Receivers) ->
    Receivers;
scan_file(Bin, Blksize, Tbl, Me, Receivers) ->
    {Rcv, Next} = split_and_find(Bin, Blksize, Me, Tbl),
    scan_file(Next, Blksize, Tbl, Me, [Rcv | Receivers]).
scan_file(Bin, Blksize, Tbl) ->
    scan_file(Bin, Blksize, Tbl, self(), []).

top_ten(D) ->
    sort(dict:to_list(D)).

start(File, Blksize) ->
    Tbl = wfbm4_tuple:init(),
    bfile:load_driver(),
    {ok, F} = bfile:fopen(File, "r"),
    {ok, Bin} = bfile:fread(F, filelib:file_size(File)),
    Receivers = scan_file(Bin, Blksize, Tbl),
    bfile:fclose(F),
    L = top_ten(receive_tbls(Receivers)),
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

sort(L) when length(L) =< 10 ->
    L;
sort(L) ->
    {A,L1} = lists:split(10, L),
    Acc = lists:keysort(2, A),
    lists:reverse(foldl(Acc,L1)).

foldl(Init,L) ->
    lists:foldr(fun ({_,X}=E, [{_,C}|Es]) when X>C-> 
			lists:keysort(2, [E|Es]);
		    (_E, Acc) ->
			Acc
		end, Init, L).
