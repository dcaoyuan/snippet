%% Parallelized file reading, don't care about tail, with binary traversing tips
-module(tbray8).

-compile([native]).

-export([start/1,
         start/2]).

%% erl -smp
%% timer:tc(tbray, start, ["o1000k.ap", 10]).
start([FileName, ProcNum]) when is_list(ProcNum) -> 
    start(FileName, list_to_integer(ProcNum)).
start(FileName, ProcNum) ->
    Start = now(),

    Main = self(),
    Counter = spawn(fun () -> count_loop(Main) end),
    Collector = spawn(fun () -> collect_loop(Counter) end),

    pread_file(FileName, ProcNum, Collector),

    %% don't terminate, wait here, until all tasks done.
    receive
        stop -> io:format("Time: ~10.2f ms~n", [timer:now_diff(now(), Start) / 1000])       
    end.

pread_file(FileName, ProcNum, Collector) ->
    ChunkSize = filelib:file_size(FileName) div ProcNum,
    pread_file_1(FileName, ChunkSize, ProcNum, Collector).
pread_file_1(FileName, ChunkSize, ProcNum, Collector) ->
    [spawn(fun () ->
                   Length = if  I == ProcNum - 1 -> ChunkSize * 2; %% latest chuck
                                true -> ChunkSize end,
                   {ok, File} = file:open(FileName, [read, binary]),
                   {ok, Bin} = file:pread(File, ChunkSize * I, Length),
                   file:close(File),
                   DataL = split_on_last_newline(Bin),
                   Collector ! {seq, I, Bin, DataL}
           end) || I <- lists:seq(0, ProcNum - 1)],
    Collector ! {chunk_num, ProcNum}.

collect_loop(Counter) -> collect_loop_1([], <<>>, -1, Counter).
collect_loop_1(Chunks, PrevTail, LastSeq, Counter) ->
    receive
        {chunk_num, ChunkNum} ->
            Counter ! {chunk_num, ChunkNum},
            collect_loop_1(Chunks, PrevTail, LastSeq, Counter);
        {seq, I, Data, Tail} ->
            SortedChunks = lists:keysort(1, [{I, Data, Tail} | Chunks]),
            {Chunks1, PrevTail1, LastSeq1} = 
                process_chunks(SortedChunks, [], PrevTail, LastSeq, Counter),
            collect_loop_1(Chunks1, PrevTail1, LastSeq1, Counter)
    end.
    
count_loop(Main) -> count_loop_1(Main, dict:new(), undefined, 0).
count_loop_1(Main, Dict, ChunkNum, ChunkNum) ->
    print_result(Dict),
    Main ! stop;
count_loop_1(Main, Dict, ChunkNum, ProcessedNum) ->
    receive
        {chunk_num, ChunkNumX} -> 
            count_loop_1(Main, Dict, ChunkNumX, ProcessedNum);
        {dict, DictX} ->
            Dict1 = dict:merge(fun (_, V1, V2) -> V1 + V2 end, Dict, DictX),
            count_loop_1(Main, Dict1, ChunkNum, ProcessedNum + 1)
    end.

process_chunks([], ChunkBuf, PrevTail, LastSeq, _) -> {ChunkBuf, PrevTail, LastSeq};
process_chunks([{I, Data, Tail}=Chunk|T], ChunkBuf, PrevTail, LastSeq, Counter) ->
    case LastSeq + 1 of
        I ->
            spawn_opt(fun () -> Counter ! {dict, scan_chunk({Data, PrevTail})} end, [{min_heap_size, 8196}]),
            process_chunks(T, ChunkBuf, Tail, I, Counter);
        _ ->
            process_chunks(T, [Chunk | ChunkBuf], PrevTail, LastSeq, Counter)
    end.

print_result(Dict) ->
    SortedList = lists:reverse(lists:keysort(2, dict:to_list(Dict))),
    [io:format("~b\t: ~s~n", [V, K]) || {K, V} <- lists:sublist(SortedList, 10)].

split_on_last_newline(Bin) -> split_on_last_newline_1(Bin, size(Bin)).   
split_on_last_newline_1(Bin, S) when S > 0 ->
    case Bin of
        <<_:S/binary,$\n,_/binary>> -> S;
        _ -> split_on_last_newline_1(Bin, S - 1)
    end;
split_on_last_newline_1(_, S) -> S.

scan_chunk({Bin, DataL}) -> scan_chunk_1(Bin, DataL, 0, dict:new()).
scan_chunk_1(Bin, DataL, S, Dict) when S < DataL - 34 ->
    case Bin of
        <<_:S/binary,"GET /ongoing/When/",_,_,_,$x,$/,_,_,_,_,$/,_,_,$/,_,_,$/,_/binary>> ->
            case match_until_space_newline(Bin, S + 34) of
                {true, E} ->
                    Skip = S + 23, L = E - Skip,
                    <<_:Skip/binary,Key:L/binary,_/binary>> = Bin,
                    scan_chunk_1(Bin, DataL, E + 1, dict:update_counter(Key, 1, Dict));
                {false, E} -> 
                    scan_chunk_1(Bin, DataL, E + 1, Dict)
            end;
        _ -> scan_chunk_1(Bin, DataL, S + 1, Dict) %% here I have to skip Bin 1 byte by 1 byte
    end;
scan_chunk_1(_, _, _, Dict) -> Dict.
    
match_until_space_newline(Bin, S) when S < size(Bin) ->
    case Bin of
        <<_:S/binary,10,_/binary>> -> {false, S};
        <<_:S/binary,$.,_/binary>> -> {false, S};
        <<_:S/binary,_,$ ,_/binary>> -> {true, S + 1};
        _ -> match_until_space_newline(Bin, S + 1)
    end;
match_until_space_newline(_, S) -> {false, S}.



