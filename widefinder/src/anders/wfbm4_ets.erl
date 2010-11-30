%% Author Anders Nygren
%% Based on Steve Vinoski's wfbm4, see below.
%% Changes: 
%% - Use tuple instead of dict to store shift values.
%%
%% wfbm4 -- search functions for Tim Bray's Wide Finder project
%% Author: Steve Vinoski (http://steve.vinoski.net/), 21 October 2007.
%% See <http://steve.vinoski.net/blog/2007/10/21/faster-wf-still/>.
-module(wfbm4_ets).
-export([find/5, find/6, init/0]).
-compile([native]).

-define(STR, "] \"GET /ongoing/When/").
-define(REVSTR, "/nehW/gniogno/ TEG\" ]").
-define(STRLEN, 21).      %length(?STR)
-define(DATELEN, 16).     %length("200x/2000/00/00/")
-define(MATCHHEADLEN, 5). %length("200x/")

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
    D = set_shifts(?STR, 0, set_defaults(lists:seq(1, 255), dict:new())),
    list_to_tuple([S||{_C,S} <-lists:sort(dict:to_list(D))]).

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
    <<Front:?DATELEN/binary, Tail/binary>> = Bin,
    case Front of
        <<_:3/binary,"x/",Y:4/binary,$/,M:2/binary,$/,D:2/binary,$/>> ->
            case check_for_dot_or_space(Tail) of
                {ok, Match} ->
                    {ok, <<Y/binary,$/,M/binary,$/,D/binary,$/, Match/binary>>};
                {nomatch, Skip} -> {skip, ?DATELEN + Skip}
            end;
        _ -> nomatch
    end.

match_front(_, -1, _, _, _) ->
    {true, 0};
match_front(Bin, Len, [C1|T], Comps, Tbl) ->
    <<_:Len/binary, C2:8, _/binary>> = Bin,
    if
        C1 =:= C2 ->
            match_front(Bin, Len-1, T, Comps+1, Tbl);
        true ->
            Shift = element(C2, Tbl),
            if
                Shift =:= ?STRLEN ->
                    {false, Shift};
                true ->
                    if
                        Comps >= Shift ->
                            {false, 1};
                        true ->
                            {false, Shift-Comps}
                    end
            end
    end.

find(Bin, Tbl, Pid, AccTo, Tab) ->
    {T, _} = timer:tc(?MODULE,find,[Bin, Tbl, Pid, AccTo, Tab, 0]),
%%    io:format("find ~p~n",[T]),
    ok.

find(Bin, _, _Pid, AccTo, _Tab, _N) when size(Bin) =< ?STRLEN ->
    AccTo ! done;
%% find(Bin, Tbl, Pid ,AccTo, 500) ->
%%     spawn_opt(fun() -> wfbm4_my:find(Bin, Tbl, Pid, AccTo) end,
%% 	      [{min_heap_size, 20000}
%% 	      ]);

find(Bin, Tbl, Pid, AccTo, Tab, N) ->
    <<Front:?STRLEN/binary, Tail/binary>> = Bin,
    case match_front(Front, ?STRLEN-1, ?REVSTR, 0, Tbl) of
        {false, Shift} ->
            <<_:Shift/binary, Next/binary>> = Bin,
            find(Next, Tbl, Pid, AccTo, Tab, N);
        {true, _} ->
            case get_tail(Tail) of
                {ok, Match} ->
                    Len = size(Match) + ?MATCHHEADLEN,
                    <<_:Len/binary, Rest/binary>> = Tail,
  		    case catch ets:update_counter(Tab, Match, 1) of
  			{'EXIT', _Rsn} -> Pid ! Match;
  			_ -> ok
  		    end,
                    find(Rest, Tbl, Pid, AccTo, Tab, N+1);
                {skip, Skip} ->
                    <<_:Skip/binary, More/binary>> = Tail,
                    find(More, Tbl, Pid, AccTo, Tab, N+1);
                nomatch ->
                    find(Tail, Tbl, Pid, AccTo, Tab, N)
            end
    end.
