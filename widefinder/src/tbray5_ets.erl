-module(tbray5_ets).

-compile([native]).

-export([start/1]).

-define(BUFFER_SIZE, (4096 * 10000)).

start(FileName) ->
    Start = now(),

    Tab = ets:new(widefinder, [set, public]),

    Main = self(),
    Collector = spawn(fun () -> collect_loop(Main, Tab) end),

    {ok, File} = file:open(FileName, [raw, binary]),    
    read_file(File, Collector),

    %% don't terminate, wait here, until all tasks done.
    receive
        stop -> io:format("Time: ~10.2f ms~n", [timer:now_diff(now(), Start) / 1000])       
    end.
    
collect_loop(Main, Tab) -> collect_loop_1(Main, Tab, undefined, 0).
collect_loop_1(Main, Tab, ChunkNum, ChunkNum) ->
    print_result(Tab),
    Main ! stop;
collect_loop_1(Main, Tab, ChunkNum, ProcessedNum) ->
    receive
        one_chunk_finished ->
            collect_loop_1(Main, Tab, ChunkNum, ProcessedNum + 1);
        {chunk_num, ChunkNumX} -> 
            collect_loop_1(Main, Tab, ChunkNumX, ProcessedNum);
        {key, Key} ->
            case catch ets:update_counter(Tab, Key, 1) of
                     N when is_integer(N) -> ok;
                     _ -> ets:insert(Tab, {Key, 1})
                 end,
                collect_loop_1(Main, Tab, ChunkNum, ProcessedNum)
   end.

print_result(Tab) ->
    SortedList = lists:reverse(ets:foldl(fun (M, Acc) when length(Acc) <10 ->
                                                 lists:keysort(2, [M | Acc]);
                                             ({_, X}=E, [{_, C}|Es]) when X > C-> 
                                                 lists:keysort(2, [E | Es]);
                                             (_, Acc) ->
                                                 Acc
                                         end, [], Tab)),
    [io:format("~b\t: ~p~n", [V, K]) || {K, V} <- SortedList].
          
read_file(File, Collector) -> read_file_1(File, 0, 0, 0, Collector).            
read_file_1(File, Offset, PrevTail, I, Collector) ->
    Offset1 = Offset - PrevTail,
    case file:pread(File, Offset1, ?BUFFER_SIZE) of
        eof -> 
            file:close(File),
            Collector ! {chunk_num, I};
        {ok, Bin} -> 
            {DataEnd, NextTail} = split_on_last_newline(Bin),
            spawn(fun () -> scan_chunk(Bin, DataEnd, Collector) end),
            read_file_1(File, Offset1 + ?BUFFER_SIZE,  NextTail, I + 1, Collector)
    end.

split_on_last_newline(Bin) -> split_on_last_newline_1(Bin, size(Bin)).   
split_on_last_newline_1(Bin, Offset) when Offset > 0 ->
    case Bin of
        <<_:Offset/binary,$\n,_/binary>> ->
            {Offset, size(Bin) - (Offset + 1)};
        _ -> 
            split_on_last_newline_1(Bin, Offset - 1)
    end;
split_on_last_newline_1(_, Offset) -> {Offset, 0}.

scan_chunk(Bin, DataEnd, Collector) -> scan_chunk_1(Bin, DataEnd, 0, Collector).
scan_chunk_1(Bin, DataEnd, Offset, Collector) when Offset < DataEnd - 34 ->
    case Bin of
        <<_:Offset/binary,"GET /ongoing/When/",_,_,_,$x,$/,_,_,_,_,$/,_,_,$/,_,_,$/,_/binary>> ->            
            case match_until_space_newline(Bin, Offset + 34) of
                {false, E} -> 
                    scan_chunk_1(Bin, DataEnd, E + 1, Collector);
                {true, E} ->
                    WordS = Offset + 23,
                    L = E - WordS,
                    <<_:WordS/binary,Key:L/binary,_/binary>> = Bin,
                    Collector ! {key, Key},
                    scan_chunk_1(Bin, DataEnd, E + 1, Collector)
            end;
        _ -> scan_chunk_1(Bin, DataEnd, Offset + 1, Collector)
    end;
scan_chunk_1(_, _, _, Collector) -> Collector ! one_chunk_finished.

match_until_space_newline(Bin, Offset) when Offset < size(Bin) ->
    case Bin of
        <<_:Offset/binary,_,$ ,_/binary>> ->
            {true, Offset + 1};
        <<_:Offset/binary,$.,_/binary>> ->
            {false, Offset};
        <<_:Offset/binary,10,_/binary>> ->
            {false, Offset};
        _ ->
            match_until_space_newline(Bin, Offset + 1)
    end;
match_until_space_newline(_, Offset) -> {false, Offset}.
    
