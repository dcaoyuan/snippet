%% Author Anders Nygren
%% Based on Steve Vinoski's wfbm4, see below.
%% Changes: 
%% - Use tuple instead of dict to store shift values.
%% - Do not bind variables when binary matching unless they will
%% be used.
%% - Removed the last binary bindings, thanks to Hynek Vychodil.
%%
%% wfbm4 -- search functions for Tim Bray's Wide Finder project
%% Author: Steve Vinoski (http://steve.vinoski.net/), 21 October 2007.
%% See <http://steve.vinoski.net/blog/2007/10/21/faster-wf-still/>.
-module(wfbm4_ets1_1).
-export([find/6, init/0]).
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

match_front(_, _, [], _, _) -> 
    {true, 0};
match_front(Bin, N, [C1 | T], Comps, Tbl) ->
    <<_:N/binary, C2:8, _/binary>> = Bin,
    case C1 of
	C2 -> match_front(Bin, N - 1, T, Comps + 1, Tbl);
	_ ->
	    case element(C2, Tbl) of
		?STRLEN -> {false, ?STRLEN};
		Shift when Comps >= Shift -> {false, 1};
		Shift -> {false, Shift - Comps}
	    end
    end.

scan_key(B, N, S) when N < S ->
    <<_:N/binary, C, _/binary>> = B,
    case C of
	$\s -> {ok, N};
	$. -> {none, N};
        $\n -> {none, N};
        _ -> scan_key(B, N + 1, S)
    end;
scan_key(_, N, _) -> {none, N}.

find(Bin, Size, Tbl, Pid, AccTo, Tab) ->
    find(Bin, Tbl, Pid, AccTo, Tab, 0,
	 Size - (?STRLEN) - (?DATELEN)),
    AccTo ! done.

find(Bin, Tbl, Pid, AccTo, Tab, N, S) when N < S ->
    case match_front(Bin, N + (?STRLEN) - 1, ?REVSTR, 0, Tbl) of
	{false, Shift} -> 
	    find(Bin, Tbl, Pid, AccTo, Tab, N + Shift, S);
	{true, _} ->
	    N1 = N + (?STRLEN),
	    N2 = N1 + 3,
	    case Bin of
		<<_:N2/binary, "x/", _:4/binary, $/, _:2/binary, $/,
		 _:2/binary, $/, _/binary>> ->
		    case scan_key(Bin, N1 + (?DATELEN),
				  S + (?STRLEN) + (?DATELEN))
			of
			{none, F} -> find(Bin, Tbl, Pid, AccTo, Tab, F, S);
			{ok, F} ->
			    N3 = N1 + (?MATCHHEADLEN),
			    L = F - N3,
			    <<_:N3/binary, Match:L/binary, _/binary>> = Bin,
			    case catch ets:update_counter(Tab, Match, 1) of
				{'EXIT', _Rsn} -> Pid ! Match;
				_ -> ok
			    end,
%%%                      safe_add_key(Key, Tab),
			    find(Bin, Tbl, Pid, AccTo, Tab, F, S)
		    end;
		_ -> find(Bin, Tbl, Pid, AccTo, Tab, N1, S)
	    end
    end;

find(_, _, _, _, _, _, _) -> 
    done.
