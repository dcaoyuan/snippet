%% Boyer-Moore searching on ASCII encoded binary
-module(bmsearch).
-export([compile/1, match/3]).

-record(bmCtx, {pat, len, tab}).

compile(Str) ->
    Len = length(Str),
    Default = dict:from_list([{C, Len} || C <- lists:seq(1, 255)]),
    Dict = set_shifts(Str, Len, 1, Default),
    Tab = list_to_tuple([Pos || {_, Pos} <- lists:sort(dict:to_list(Dict))]),
    #bmCtx{pat = lists:reverse(Str), len = Len, tab = Tab}.

set_shifts([], _, _, Dict) -> Dict;
set_shifts([C|T], StrLen, Pos, Dict) ->
    set_shifts(T, StrLen, Pos + 1, dict:store(C, StrLen - Pos, Dict)).

%% @spec match(Bin, Start, #bmCtx) -> {true, Len} | {false, SkipLen}
match(Bin, S, #bmCtx{pat=Pat, len=Len, tab=Tab}) -> 
    match_1(Bin, S + Len - 1, Pat, Len, Tab, 0).
match_1(Bin, S, [C|T], Len, Tab, Count) ->
    <<_:S/binary, C1, _/binary>> = Bin,
    case C1 of
        C -> 
            match_1(Bin, S - 1, T, Len, Tab, Count + 1);
        _ ->    
            case element(C1, Tab) of
                Len -> {false, Len};
                Shift when Shift =< Count -> {false, 1};
                Shift -> {false, Shift - Count}
            end
    end;
match_1(_, _, [], Len, _, _) -> {true, Len}.
