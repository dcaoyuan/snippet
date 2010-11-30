%% Author: Caoyuan Deng, Anders Nygreni, Steve Vinoski
%% return keys, use dict
-module(tbray9b).
-compile([native, inline]).
-export([start/1]).

-define(BUFFER_SIZE, (1024 * 50000)).
-define(PAT, "/nehW/gniogno/ TEG\" ]"). % lists:reverse("] \"GET /ongoing/When/").
-define(PAT_LEN, 21).    % length("/nehW/gniogno/ TEG\" ]").
-define(SKIP_LEN, 26).   % length("] \"GET /ongoing/When/200x/").
-define(KEY_OFFSET, 37). % length("] \"GET /ongoing/When/200x/2000/10/10/").

start([FileName]) ->
    FileSize = filelib:file_size(FileName),
    ProcN = FileSize div ?BUFFER_SIZE,
    ProcN1 = case FileSize rem ?BUFFER_SIZE of 0 -> ProcN; _ -> ProcN + 1 end,
    BmCtx = bm_init(),
    Results = [wait_result(Worker) || Worker <- read_file(FileName, ?BUFFER_SIZE, ProcN1, BmCtx)],
    {Dict, Segs} = lists:foldl(fun ({Keys, Head, Tail}, {Dict, Segs}) ->
                                       {update_dict(Dict, Keys), [Head, Tail | Segs]}
                               end, {dict:new(), []}, Results),
    Segs1 = [Seg || {_, Seg} <- lists:keysort(1, Segs)],
    SegKeys = scan_chunk({list_to_binary(Segs1), BmCtx}),
    Dict1 = update_dict(Dict, SegKeys),
    print_result(Dict1),
    halt().

update_dict(Dict, Keys) ->
    lists:foldl(fun (Key, D) -> dict:update_counter(Key, 1, D) end, Dict, Keys).

read_file(FileName, Size, ProcN, BmCtx) ->
    [spawn_worker(self(), fun scan_file/1, {FileName, Size, I, BmCtx})
     || I <- lists:seq(0, ProcN - 1)].
 
scan_file({FileName, Size, I, BmCtx}) ->
    {ok, File} = file:open(FileName, [raw, binary]),
    {ok, Bin} = file:pread(File, Size * I, Size),
    file:close(File),
    HeadL = split_on_first_newline(Bin),
    TailS = split_on_last_newline(Bin),
    DataL = TailS - HeadL,
    <<Head:HeadL/binary, Data:DataL/binary, Tail/binary>> = Bin,
    {scan_chunk({Data, BmCtx}), {I * 10, Head}, {I * 10 + 1, Tail}}.

split_on_first_newline(Bin) -> split_on_first_newline_1(Bin, 0).
split_on_first_newline_1(Bin, S) ->
    case Bin of
        <<_:S/binary,$\n,_/binary>> -> S + 1;
        _ -> split_on_first_newline_1(Bin, S + 1)
    end.

split_on_last_newline(Bin) -> split_on_last_newline_1(Bin, size(Bin)).   
split_on_last_newline_1(Bin, S) when S > 0 ->
    case Bin of
        <<_:S/binary,$\n,_/binary>> -> S + 1;
        _ -> split_on_last_newline_1(Bin, S - 1)
    end;
split_on_last_newline_1(_, S) -> S.
    
scan_chunk({Bin, BmCtx}) -> scan_chunk_1(Bin, size(Bin), 0, [], BmCtx).
scan_chunk_1(Bin, DataL, S, Dict, BmCtx) when S < DataL - ?KEY_OFFSET ->
    case bm_match(Bin, S, BmCtx) of
        {true, _} ->
            case match_until_space_newline(Bin, S + ?KEY_OFFSET, size(Bin)) of
                {true, E} ->
                    Skip = S + ?SKIP_LEN, L = E - Skip,
                    <<_:Skip/binary, Key:L/binary, _/binary>> = Bin,
                    scan_chunk_1(Bin, DataL, E + 1, [Key | Dict], BmCtx);
                {false, E} -> 
                    scan_chunk_1(Bin, DataL, E + 1, Dict, BmCtx)
            end;
        {false, Shift} -> 
            scan_chunk_1(Bin, DataL, S + Shift, Dict, BmCtx)
    end;
scan_chunk_1(_, _, _, Dict, _) -> Dict.

match_until_space_newline(Bin, S, DataL) when S < DataL ->
    case Bin of
        <<_:S/binary,10,_/binary>> -> {false, S};
        <<_:S/binary,$.,_/binary>> -> {false, S};
        <<_:S/binary,_,$ ,_/binary>> -> {true, S + 1};
        _ -> match_until_space_newline(Bin, S + 1, DataL)
    end;
match_until_space_newline(_, S, _) -> {false, S}.
    
spawn_worker(Parent, F, A) ->
    erlang:spawn_monitor(fun() -> Parent ! {self(), F(A)} end).

wait_result({Pid, Ref}) ->
    receive
        {'DOWN', Ref, _, _, normal} -> receive {Pid, Result} -> Result end;
        {'DOWN', Ref, _, _, Reason} -> exit(Reason)
    end.

print_result(Dict) ->
    SortedList = lists:reverse(lists:keysort(2, dict:to_list(Dict))),
    [io:format("~b\t: ~s~n", [V, K]) || {K, V} <- lists:sublist(SortedList, 10)].
    
bm_init() ->
    Default = dict:from_list([{C, ?PAT_LEN} || C <- lists:seq(1, 255)]),
    Dict = bm_set_shifts(lists:reverse(?PAT), ?PAT_LEN, 1, Default),
    list_to_tuple([Pos || {_, Pos} <- lists:sort(dict:to_list(Dict))]).

bm_set_shifts([], _, _, Dict) -> Dict;
bm_set_shifts([C|T], StrLen, Pos, Dict) ->
    bm_set_shifts(T, StrLen, Pos + 1, dict:store(C, StrLen - Pos, Dict)).

bm_match(Bin, S, Tab) -> bm_match_1(Bin, S + ?PAT_LEN - 1, ?PAT, Tab, 0).
bm_match_1(Bin, S, [C|T], Tab, Count) ->
    <<_:S/binary, C1, _/binary>> = Bin,
    case C1 of
        C ->
            bm_match_1(Bin, S - 1, T, Tab, Count + 1);
        _ ->    
            case element(C1, Tab) of
                ?PAT_LEN -> {false, ?PAT_LEN};
                Shift when Shift =< Count -> {false, 1};
                Shift -> {false, Shift - Count}
            end
    end;
bm_match_1(_, _, [], _, _) -> {true, ?PAT_LEN}.  