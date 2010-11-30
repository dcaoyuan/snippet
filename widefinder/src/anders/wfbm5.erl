%% Author Anders Nygren
%% Based on Steve Vinoski's wfbm4, see below.
%% Changes: 
%% - Use tuple instead of dict to store shift values.
%% - Do not bind variables when binary matching unless they will
%% be used.
%%
%% wfbm4 -- search functions for Tim Bray's Wide Finder project
%% Author: Steve Vinoski (http://steve.vinoski.net/), 21 October 2007.
%% See <http://steve.vinoski.net/blog/2007/10/21/faster-wf-still/>.
-module(wfbm5).
-export([find/5, init/0]).
-compile([native]).

-define(STR,    "] \"GET /ongoing/When/200x/****/**/**/").
-define(REVSTR, "/**/**/****/x002/nehW/gniogno/ TEG\" ]").
-define(STRLEN, 37).      %length(?STR)
-define(SKIP_LEN, 26).     %length("] \"GET /ongoing/When/200x/")
%-define(STR,    "] \"GET /ongoing/When/").
%-define(REVSTR, "/nehW/gniogno/ TEG\" ]").
%-define(STRLEN, 21).      %length(?STR)
%-define(SKIP_LEN, 26).     %length("] \"GET /ongoing/When/200x/")

init() ->
    Dict = set_shifts(?STR, 0, set_defaults(lists:seq(1, 255), dict:new())),
    list_to_tuple([Pos || {_, Pos} <- lists:sort(dict:to_list(Dict))]).

set_defaults([], Dict) -> Dict;
set_defaults([C|T], Dict) ->
    set_defaults(T, dict:store(C, ?STRLEN, Dict)).

set_shifts([], _, Dict) -> Dict;
set_shifts([C|T], Pos, Dict) ->
    set_shifts(T, Pos + 1, dict:store(C, ?STRLEN - Pos - 1, Dict)).

find(AccTo, Bin, Tbl, Pid, Tab) ->
    find(Bin, Tbl, Pid, Tab, 0, size(Bin) - ?STRLEN),
    AccTo ! done.

find(Bin, Tbl, Pid, Tab, S, DataL) when S < DataL ->
    case match_front(Bin, S + ?STRLEN - 1, ?REVSTR, 0, Tbl) of
	true ->
            case scan_key(Bin, S + ?STRLEN, size(Bin)) of
                {none, E} -> 
                    find(Bin, Tbl, Pid, Tab, E + 1, DataL);
                {ok, E} ->
                    Skip = S + ?SKIP_LEN, L = E - Skip,
                    <<_:Skip/binary,Key:L/binary,_/binary>> = Bin,
                    case catch ets:update_counter(Tab, Key, 1) of
                        {'EXIT', _} -> 
                            ets:insert(Tab, {Key, 1});
                        _ -> ok
                    end,
                    find(Bin, Tbl, Pid, Tab, E + 1, DataL)
            end;
	{false, Shift} -> find(Bin, Tbl, Pid, Tab, S + Shift, DataL)
    end;
find(_, _, _, _, _, _) -> done.

match_front(Bin, S, [C|T], Count, Tab) ->
    <<_:S/binary,C1,_/binary>> = Bin,
    case C of
        $* when C1 >= $0 andalso C1 =< $9 -> 
            match_front(Bin, S - 1, T, Count + 1, Tab);
        C1 when C1 /= $* ->
            match_front(Bin, S - 1, T, Count + 1, Tab);
        _  when C1 >= $0 andalso C1 =< $9 -> %% it's digital, should shift to $*
            get_shift($*, Tab, Count);
        _ -> 
            get_shift(C1, Tab, Count)
    end;
match_front(_, _, [], _, _) -> true.

get_shift(C, Tab, Comped) ->
    case element(C, Tab) of
        ?STRLEN -> {false, ?STRLEN};
        Shift when Shift =< Comped -> {false, 1};
        Shift -> {false, Shift - Comped}
    end.

scan_key(Bin, S, DataL) when S < DataL ->
    case Bin of
	<<_:S/binary,$.,_/binary>> -> {none, S};
        <<_:S/binary,10,_/binary>> -> {none, S};
	<<_:S/binary,_,$ ,_/binary>> -> {ok, S + 1};
        _ -> scan_key(Bin, S + 1, DataL)
    end;
scan_key(_, S, _) -> {none, S}.





