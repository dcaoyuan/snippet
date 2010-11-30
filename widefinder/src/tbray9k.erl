%% Author: Caoyuan Deng, Anders Nygreni, Steve Vinoski
-module(tbray9k).
-compile([native, inline]).
-export([start/1]).

-define(READ_SIZE, (1024 * 50000)).
-define(PAT, "/nehW/gniogno/ TEG\" "). % lists:reverse("] \"GET /ongoing/When/").
-define(PAT_LEN, 20).    % length("/nehW/gniogno/ TEG\" ]").
-define(SKIP_LEN, 25).   % length("] \"GET /ongoing/When/200x/").
-define(KEY_OFFSET, 36). % length("] \"GET /ongoing/When/200x/2000/10/10/").

start([FileName]) ->
    FileSize = filelib:file_size(FileName),
    ProcN = FileSize div ?READ_SIZE,
    ProcN1 = case FileSize rem ?READ_SIZE of 0 -> ProcN; _ -> ProcN + 1 end,
    BmCtx = bm_compile(),
    Results = scan_file(FileName, ?READ_SIZE, ProcN1, BmCtx),                             
    Ets = ets:new(wfets, [set]),
    Segs = lists:foldl(fun ({Keys, Head, Tail}, Segs) ->
                               lists:foreach(fun (Key) -> update_counter(Ets, Key) end, Keys),
                               [Head | [Tail | Segs]]
                       end, [], Results),
    Segs1 = [Seg || {_, Seg} <- lists:keysort(1, Segs)], 
    SegKeys = scan_chunk([list_to_binary(Segs1), BmCtx]),
    lists:foreach(fun (Key) -> update_counter(Ets, Key) end, SegKeys),
    print_result(Ets),
    halt().

update_counter(Ets, Key) ->
    case catch ets:update_counter(Ets, Key, 1) of
        {'EXIT', {badarg, _}} -> ets:insert(Ets, {Key, 1});
        _ -> ok
    end.

scan_file(FileName, Size, ProcN, BmCtx) ->
    Workers = [spawn_worker(self(), fun pscan_file/1, [FileName, Size, I, BmCtx], [])
               || I <- lists:seq(0, ProcN - 1)],
    [wait_result(Worker) || Worker <- Workers].

pscan_file([FileName, Size, I, BmCtx]) ->
    {ok, File} = file:open(FileName, [read, raw, binary]),
    {ok, Bin} = file:pread(File, Size * I, Size),
    file:close(File),
    HeadL = split_on_first_newline(Bin, 0, 1),
    TailS = split_on_first_newline(Bin, size(Bin) - 1, -1),
    DataL = TailS - HeadL,
    <<Head:HeadL/binary, Data:DataL/binary, Tail/binary>> = Bin,
    {scan_chunk([Data, BmCtx]), {I * 10, Head}, {I * 10 + 1, Tail}}.

split_on_first_newline(Bin, S, Direction) ->
    case Bin of
        <<_:S/binary,$\n,_/binary>> -> S + 1;
        <<_:S/binary,_,_/binary>> -> split_on_first_newline(Bin, S + Direction, Direction);
        _ -> S
    end.
        
scan_chunk([Bin, BmCtx]) -> scan_chunk_1(Bin, size(Bin), 0, [], BmCtx).
scan_chunk_1(Bin, DataL, S, Keys, BmCtx) when S < DataL - ?KEY_OFFSET ->
    case bm_match(Bin, S + ?PAT_LEN, ?PAT, BmCtx, 1) of
        {true, _} ->
            case match_until_space_newline(Bin, S + ?KEY_OFFSET, size(Bin)) of
                {true, E} ->
                    Skip = S + ?SKIP_LEN, L = E - Skip,
                    <<_:Skip/binary, Key:L/binary, _/binary>> = Bin,
                    scan_chunk_1(Bin, DataL, E, [Key | Keys], BmCtx);
                {false, E} -> 
                    scan_chunk_1(Bin, DataL, E, Keys, BmCtx)
            end;
        {false, Shift} -> 
            scan_chunk_1(Bin, DataL, S + Shift, Keys, BmCtx)
    end;
scan_chunk_1(_, _, _, Keys, _) -> Keys.

match_until_space_newline(Bin, S, DataL) when S < DataL ->
    case Bin of
        <<_:S/binary,10,_/binary>> -> {false, S};
        <<_:S/binary,$.,_/binary>> -> {false, S};
        <<_:S/binary,_,$ ,_/binary>> -> {true, S + 1};
        _ -> match_until_space_newline(Bin, S + 1, DataL)
    end;
match_until_space_newline(_, S, _) -> {false, S}.
    
print_result(Tab) ->
    SortedList = lists:reverse(lists:keysort(2, ets:tab2list(Tab))),
    [io:format("~b\t: ~s~n", [V, K]) || {K, V} <- lists:sublist(SortedList, 10)].

spawn_worker(Parent, F, Args, Opts) ->
    erlang:spawn_opt(fun() -> Parent ! {self(), F(Args)} end, [monitor | Opts]).

wait_result({Pid, Ref}) ->
    receive
        {'DOWN', Ref, _, _, normal} -> receive {Pid, Result} -> Result end;
        {'DOWN', Ref, _, _, Reason} -> exit(Reason)
    end.
    
bm_compile() ->
    Default = dict:from_list([{C, ?PAT_LEN} || C <- lists:seq(1, 255)]),
    Dict = bm_set_shifts(lists:reverse(?PAT), ?PAT_LEN, 1, Default),
    list_to_tuple([Pos || {_, Pos} <- lists:sort(dict:to_list(Dict))]).

bm_set_shifts([C|T], StrLen, Pos, Dict) ->
    bm_set_shifts(T, StrLen, Pos + 1, dict:store(C, StrLen - Pos, Dict));
bm_set_shifts([], _, _, Dict) -> Dict.

bm_match(Bin, S, [P1,P2|T], Tab, Count) ->
    S1 = S - 1,
    case Bin of
        <<_:S1/binary,P2,P1, _/binary>> ->
            bm_match(Bin, S - 2, T, Tab, Count + 2);
        <<_:S1/binary,C,P1,_/binary>> ->
            case element(C, Tab) of
                ?PAT_LEN -> {false, ?PAT_LEN};
                Shift when Shift < Count -> {false, 1};
                Shift -> {false, Shift - Count + 1}
            end;
        <<_:S1/binary,_,C,_/binary>> ->
            case element(C, Tab) of
                ?PAT_LEN -> {false, ?PAT_LEN};
                Shift when Shift < Count -> {false, 1};
                Shift -> {false, Shift - Count + 1}
            end
    end;
bm_match(_, _, [], _, _) -> {true, ?PAT_LEN}.  