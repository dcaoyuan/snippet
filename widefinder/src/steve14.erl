% tbray14 -- another Erlang solution to Tim Bray's Wide Finder project
% Author: Steve Vinoski (http://steve.vinoski.net/), 14 October 2007.
% See <http://steve.vinoski.net/blog/2007/10/14/one-more-erlang-wide-finder/>
-module(steve14).
-export([start/2, start/3, main/1]).
-import(wfbm).
-compile([native]).

-define(READSIZE, 8192*1024).

make_tbl(L) ->
    make_tbl(L, dict:new()).
make_tbl([], Tbl) ->
    Tbl;
make_tbl([H|T], Tbl) ->
    make_tbl(T, dict:update_counter(H, 1, Tbl)).

top_ten(D) ->
    L = lists:sort(fun({_,V1}, {_,V2}) -> V1 > V2 end, dict:to_list(D)),
    if
        length(L) > 10 ->
            {First, _} = lists:split(10, L),
            First;
        true -> L
    end.

process_binary(Pid, Bin, Tbl) ->
    spawn(fun() -> Pid ! make_tbl(wfbm:find(Bin, Tbl)) end).

break_chunk_on_newline(Bin, Pos, All) when Pos >= size(Bin) -> {All, Bin};
break_chunk_on_newline(Bin, Pos, All) ->
    {_, <<C:8, _/binary>>} = split_binary(Bin, Pos),
    case C of
        $\n ->
            {Ba, Bb} = split_binary(Bin, Pos+1),
            break_chunk_on_newline(Bb, Pos, [Ba | All]);
        _ -> break_chunk_on_newline(Bin, Pos+1, All)
    end.
break_chunk_on_newline(Bin, N) -> break_chunk_on_newline(Bin, size(Bin) div N, []).

receive_tbls(L) ->
    lists:foldl(fun(_, D1) -> receive D2 ->
                                      dict:merge(fun(_,V1,V2) -> V1 + V2 end, D1, D2)
                              end end, dict:new(), L).
    
spawn_collector(Bins, Me, Tbl) ->
    Collector = spawn(fun() -> Me ! receive_tbls(Bins) end),
    [process_binary(Collector, B, Tbl) || B <- Bins],
    Collector.

scan_finish(<<>>, _, _, Pids) -> Pids;
scan_finish(More, Tbl, Me, Pids) -> [spawn_collector([More], Me, Tbl) | Pids].

scan_file(F, N, Readsize, Tbl, Me, Leftover, Pids) ->
    Rd = bfile:fread(F, Readsize),
    case Rd of
        {ok, Bin} ->
            {Bins, More} = break_chunk_on_newline(list_to_binary([Leftover, Bin]), N),
            scan_file(F, N, Readsize, Tbl, Me, More, [spawn_collector(Bins, Me, Tbl) | Pids]);
        eof -> scan_finish(Leftover, Tbl, Me, Pids)
    end.
scan_file(F, N, Readsize, Tbl) ->
    scan_file(F, N, Readsize, Tbl, self(), <<>>, []). 

start(Num, File, Readsize) ->
    Tbl = wfbm:init(),
    bfile:load_driver(),
    {ok, F} = bfile:fopen(File, "r"),
    Pids = scan_file(F, Num, Readsize, Tbl),
    bfile:fclose(F),
    L = top_ten(receive_tbls(Pids)),
    lists:map(fun({K,V}) -> io:format("~p: ~s~n", [V, K]) end, L). 
start(Num, File) ->
    start(Num, File, ?READSIZE).
start(File) ->
    start(2, File, ?READSIZE).

main([N, F, Rd]) ->
    start(list_to_integer(N), F, list_to_integer(Rd)),
    halt();
main([N, F]) ->
    start(list_to_integer(N), F),
    halt();
main([F]) ->
    start(F),
    halt().

% wfbm -- search functions for Tim Bray's Wide Finder project
% Author: Steve Vinoski (http://steve.vinoski.net/), 14 October 2007.
% See <http://steve.vinoski.net/blog/2007/10/14/one-more-erlang-wide-finder/>
-module(wfbm).
-export([find/2, init/0]).
-compile([native]).

-define(STR, "GET /ongoing/When").
-define(REVSTR, "mehW/gniogno/ TEG").
-define(STRLEN, length(?STR)).
-define(MATCHHEADLEN, length("/200x/")).
-define(SKIP, length("/200x/2007/10/15/")).

set_shifts(_, Count, Tbl) when Count =:= ?STRLEN - 1 ->
    Tbl;
set_shifts([H|T], Count, Tbl) ->
    New = ?STRLEN - Count - 1,
    NTbl = dict:store(H, New, Tbl),
    set_shifts(T, Count+1, NTbl).

set_defaults([], Tbl) ->
    Tbl;
set_defaults([V|T], Tbl) ->
    set_defaults(T, dict:store(V, ?STRLEN, Tbl)).

init() ->
    set_shifts(?STR, 0, set_defaults(lists:seq(1, 255), dict:new())).

find(Bin, Tbl) ->
    find_matches(Bin, Tbl, []).

get_tail(<<>>) ->
    no_match;
get_tail(Bin) ->
    get_tail(Bin, none, 0).
get_tail(<<"/20",_:8,"x/",_:32,$/,_:16,$/,_:16,$/, Rest/binary>>, _, _) when size(Rest) =:= 0 ->
    no_match;
get_tail(<<"/20",_:8,"x/",_:32,$/,_:16,$/,_:16,$/,32:8, _/binary>>, _, _) ->
    no_match;
get_tail(<<"/19",_:8,"x/",_:32,$/,_:16,$/,_:16,$/, Rest/binary>>, _, _) when size(Rest) =:= 0 ->
    no_match;
get_tail(<<"/19",_:8,"x/",_:32,$/,_:16,$/,_:16,$/,32:8, _/binary>>, _, _) ->
    no_match;
get_tail(<<"/20",_:8,"x/",_:32,$/,M1:8,M0:8,$/,D1:8,D0:8,$/, Rest/binary>>, none, Len)
  when ((M1-$0)*10 + (M0-$0)) =< 12, ((D1-$0)*10 + (D0-$0)) =< 31 ->
    get_tail(Rest, almost, Len+?SKIP);
get_tail(<<"/19",_:8,"x/",_:32,$/,M1:8,M0:8,$/,D1:8,D0:8,$/, Rest/binary>>, none, Len)
  when ((M1-$0)*10 + (M0-$0)) =< 12, ((D1-$0)*10 + (D0-$0)) =< 31 ->
    get_tail(Rest, almost, Len+?SKIP);
get_tail(<<32:8, _/binary>>, found, Len) ->
    {ok, Len};
get_tail(<<32:8, _/binary>>, _, _) ->
    no_match;
get_tail(<<$., _/binary>>, _, _) ->
    no_match;
get_tail(<<_:8, Rest/binary>>, almost, Len) ->
    get_tail(Rest, found, Len+1);
get_tail(<<_:8, Rest/binary>>, State, Len) ->
    get_tail(Rest, State, Len+1).

get_shift([C1|T1], [C2|T2], Tbl) when C1 =:= C2 ->
    get_shift(T1, T2, Tbl);
get_shift([C1|_], _, Tbl) ->
    dict:fetch(C1, Tbl).

find_matches(<<?STR, Tail/binary>>, Tbl, Acc) ->
    case get_tail(Tail) of
        {ok, More} ->
            {H, Rest} = split_binary(Tail, More),
            {_, Match} = split_binary(H, ?MATCHHEADLEN),
            Result = binary_to_list(Match),
            find_matches(Rest, Tbl, [Result | Acc]);
        no_match ->
            find_matches(Tail, Tbl, Acc)
    end;
find_matches(Bin, _, Acc) when size(Bin) < ?STRLEN ->
    Acc;
find_matches(Bin, Tbl, Acc) ->
    {Front, _} = split_binary(Bin, ?STRLEN),
    Shift = get_shift(lists:reverse(binary_to_list(Front)), ?REVSTR, Tbl),
    {_, Next} = split_binary(Bin, Shift),
    find_matches(Next, Tbl, Acc).

