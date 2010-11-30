%% Author: Caoyuan Deng, Anders Nygreni, Steve Vinoski
%% readin READ_SIZE, split to CHUNK_SIZE (2-layer processes)
-module(tbray10).
-compile([native, inline]).
-export([start/1]).

-define(READ_SIZE, (1024 * 50000)).
-define(CHUNK_SIZE, 20480).
-define(PAT, "/nehW/gniogno/ TEG\" ]"). % lists:reverse("] \"GET /ongoing/When/").
-define(PAT_LEN, 21).    % length("/nehW/gniogno/ TEG\" ]").
-define(SKIP_LEN, 26).   % length("] \"GET /ongoing/When/200x/").
-define(KEY_OFFSET, 37). % length("] \"GET /ongoing/When/200x/2000/10/10/").

start([FileName]) ->
    FileSize = filelib:file_size(FileName),
    ReaderN = FileSize div ?READ_SIZE,
    ReaderN1 = case FileSize rem ?READ_SIZE of 0 -> ReaderN; _ -> ReaderN + 1 end,
    BmCtx = bm_compile(),
    Results = lists:append(read_file(FileName, ?READ_SIZE, ReaderN1, BmCtx)),
    Ets = ets:new(wftab, [set]),
    Segs = lists:foldl(fun ({Keys, Head, Tail}, Segs) ->
                               lists:foreach(fun (Key) -> update_tab(Ets, Key) end, Keys),
                               [Head | [Tail | Segs]]
                       end, [], Results),
    Segs1 = [Seg || {_, Seg} <- lists:keysort(1, Segs)],
    SegKeys = scan_chunk([list_to_binary(Segs1), BmCtx]),
    lists:foreach(fun (Key) -> update_tab(Ets, Key) end, SegKeys),
    print_result(Ets),
    halt().

update_tab(Ets, Key) ->
    case catch ets:update_counter(Ets, Key, 1) of
        {'EXIT', {badarg, _}} -> ets:insert(Ets, {Key, 1});
        _ -> ok
    end.

read_file(FileName, Size, ProcN, BmCtx) ->
    Workers = [spawn_worker(self(), fun pread_file/1, [FileName, Size, I, BmCtx], [])
               || I <- lists:seq(0, ProcN - 1)],
    [wait_result(Worker) || Worker <- Workers].

pread_file([FileName, Size, I, BmCtx]) ->
    {ok, File} = file:open(FileName, [read, raw, binary]),
    {ok, Bin} = file:pread(File, Size * I, Size),
    file:close(File),
    BinSize = size(Bin),
    ChunkN = BinSize div ?CHUNK_SIZE,
    ChunkN1 = case BinSize rem ?CHUNK_SIZE of 0 -> ChunkN; _ -> ChunkN + 1 end,
    Workers = [spawn_worker(self(), fun split_bin/1, [Bin, I, J, BmCtx], [{min_heap_size, 10240}])
               || J <- lists:seq(0, ChunkN1 - 1)],
    [wait_result(Worker) || Worker <- Workers].

split_bin([Bin, I, J, BmCtx]) ->
    S = J * ?CHUNK_SIZE, L = ?CHUNK_SIZE,
    Chunk = case Bin of
                <<_:S/binary, ChunkX:L/binary, _/binary>> -> ChunkX;
                <<_:S/binary, ChunkX/binary>> -> ChunkX
            end,
    HeadL = split_on_first_newline(Chunk, 0, 1),
    TailS = split_on_first_newline(Chunk, size(Chunk) - 1, -1),
    DataL = TailS - HeadL,
    <<Head:HeadL/binary, Data:DataL/binary, Tail/binary>> = Chunk,
    {scan_chunk([Data, BmCtx]), {I * 1000 + J * 10, Head}, {I * 1000 + J * 10 + 1, Tail}}.

split_on_first_newline(Bin, S, Direction) ->
    case Bin of
        <<_:S/binary,$\n,_/binary>> -> S + 1;
        <<_:S/binary,_,_/binary>> -> split_on_first_newline(Bin, S + Direction, Direction);
        _ -> S
    end.
    
scan_chunk([Bin, BmCtx]) -> scan_chunk_1(Bin, size(Bin), 0, [], BmCtx).
scan_chunk_1(Bin, DataL, S, Keys, BmCtx) when S < DataL - ?KEY_OFFSET ->
    case bm_match(Bin, S + ?PAT_LEN - 1, ?PAT, BmCtx, 0) of
        {true, _} ->
            case match_until_space_newline(Bin, S + ?KEY_OFFSET, size(Bin)) of
                {true, E} ->
                    Skip = S + ?SKIP_LEN, L = E - Skip,
                    <<_:Skip/binary, Key:L/binary, _/binary>> = Bin,
                    scan_chunk_1(Bin, DataL, E + 1, [Key | Keys], BmCtx);
                {false, E} -> 
                    scan_chunk_1(Bin, DataL, E + 1, Keys, BmCtx)
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

bm_match(Bin, S, [C|T], Tab, Count) ->
    case Bin of
        <<_:S/binary, C, _/binary>> ->
            bm_match(Bin, S - 1, T, Tab, Count + 1);
        <<_:S/binary, C1, _/binary>> ->    
            case element(C1, Tab) of
                ?PAT_LEN -> {false, ?PAT_LEN};
                Shift when Shift < Count -> {false, 1};
                Shift -> {false, Shift - Count + 1}
            end
    end;
bm_match(_, _, [], _, _) -> {true, ?PAT_LEN}.  