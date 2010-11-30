-module(steve15).
% tbray15 -- another Erlang solution to Tim Bray's Wide Finder project
% Author: Steve Vinoski (http://steve.vinoski.net/), 18 October 2007.
% See <http://steve.vinoski.net/blog/2007/10/18/ok-just-one-more-wf/>.
-export([start/1, start/2, main/1]).
-import(wfbm3).
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
            Rcv = spawn(fun() -> receive_matches(Pid, dict:new()) end),
            spawn(fun() -> wfbm3:find(Front, Tbl, Rcv) end),
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
    L = lists:sort(fun({_,V1}, {_,V2}) -> V1 > V2 end, dict:to_list(D)),
    if
        length(L) > 10 ->
            {First, _} = lists:split(10, L),
            First;
        true -> L
    end.

start(File, Blksize) ->
    Tbl = wfbm3:init(),
    bfile:load_driver(),
    {ok, F} = bfile:fopen(File, "r"),
    {ok, Bin} = bfile:fread(F, filelib:file_size(File)),
    Receivers = scan_file(Bin, Blksize, Tbl),
    bfile:fclose(F),
    L = top_ten(receive_tbls(Receivers)),
    lists:map(fun({K,V}) -> io:format("~p: ~s~n", [V, K]) end, L).
start(File) ->
    Blksize = filelib:file_size(File) div (erlang:system_info(schedulers)*32),
    start(File, Blksize).

main([F, Blksize]) ->
    start(F, list_to_integer(Blksize)),
    halt();
main([F]) ->
    start(F),
    halt().

% wfbm3 -- search functions for Tim Bray's Wide Finder project
% Author: Steve Vinoski (http://steve.vinoski.net/), 18 October 2007.
% See <http://steve.vinoski.net/blog/2007/10/18/ok-just-one-more-wf/>.
-module(wfbm3).
-export([find/3, init/0]).
-compile([native]).

-define(STR, "] \"GET /ongoing/When/").
-define(REVSTR, "/nehW/gniogno/ TEG\" ]").
-define(STRLEN, length(?STR)).
-define(DATELEN, length("200x/2000/00/00/")).
-define(MATCHHEADLEN, length("200x/")).

set_shifts(_, Count, Tbl) when Count =:= ?STRLEN - 1 ->
    Tbl;
set_shifts([H|T], Count, Tbl) ->
    Shift = ?STRLEN - Count - 1,
    set_shifts(T, Count+1, dict:store(H, Shift, Tbl)).

set_defaults([], Tbl) ->
    Tbl;
set_defaults([V|T], Tbl) ->
    set_defaults(T, dict:store(V, ?STRLEN, Tbl)).

init() ->
    set_shifts(?STR, 0, set_defaults(lists:seq(1, 255), dict:new())).

check_for_dot_or_space(Bin) ->
    check_for_dot_or_space(Bin, 0).
check_for_dot_or_space(<<$ , _/binary>>, 0) ->
    {nomatch, 0};
check_for_dot_or_space(Bin, Len) ->
    case Bin of
        <<Front:Len/binary, $ , _/binary>> ->
            {ok, Front};
        <<_:Len/binary, $., _/binary>> ->
            {nomatch, Len};
        _ ->
            check_for_dot_or_space(Bin, Len+1)
    end.

get_tail(<<>>) ->
    nomatch;
get_tail(Bin) ->
    Frontlen = ?DATELEN,
    <<Front:Frontlen/binary, Tail/binary>> = Bin,
    case Front of
        <<_:3/binary,"x/",Y:4/binary,$/,M:2/binary,$/,D:2/binary,$/>> ->
            case check_for_dot_or_space(Tail) of
                {ok, Match} ->
                    {ok, <<Y/binary,$/,M/binary,$/,D/binary,$/, Match/binary>>};
                {nomatch, Skip} -> {skip, Frontlen + Skip}
            end;
        _ -> nomatch
    end.

match_front(_, -1, _, _) ->
    {true, 0};
match_front(Bin, Len, [C1|T], Tbl) ->
    <<_:Len/binary, C2:8, _/binary>> = Bin,
    if
        C1 =:= C2 ->
            match_front(Bin, Len-1, T, Tbl);
        true ->
            Shift = dict:fetch(C2, Tbl),
            if
                Shift =:= ?STRLEN ->
                    {false, Shift};
                true ->
                    {false, lists:min([dict:fetch(C1, Tbl), Shift])}
            end
    end.

find(Bin, _, Pid) when size(Bin) =< ?STRLEN ->
    Pid ! done;
find(Bin, Tbl, Pid) ->
    SLen = ?STRLEN,
    <<Front:SLen/binary, Tail/binary>> = Bin,
    case match_front(Front, SLen-1, ?REVSTR, Tbl) of
        {false, Shift} ->
            <<_:Shift/binary, Next/binary>> = Bin,
            find(Next, Tbl, Pid);
        {true, _} ->
            case get_tail(Tail) of
                {ok, Match} ->
                    Len = size(Match) + ?MATCHHEADLEN,
                    <<_:Len/binary, Rest/binary>> = Tail,
                    Pid ! Match,
                    find(Rest, Tbl, Pid);
                {skip, Skip} ->
                    <<_:Skip/binary, More/binary>> = Tail,
                    find(More, Tbl, Pid);
                nomatch ->
                    find(Tail, Tbl, Pid)
            end
    end.

