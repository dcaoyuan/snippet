-module(tbray_bin_no_line).

-compile([native]).

-export([start/1]).

-define(BUFFER_SIZE, (4096 * 10000)).


%% 30 seconds

%% erl -smp enable +P 60000
%% timer:tc(tbray, start, ["o1000k.ap"]).   
start(FileName) ->
    Start = now(),

    Main = self(),
    Collector = spawn(fun () -> collect_loop(Main) end),

    {ok, File} = file:open(FileName, [raw, binary]),    
    read_file(File, Collector),

    %% don't terminate, wait here, until all tasks done.
    receive
        stop -> io:format("Time: ~p ms~n", [timer:now_diff(now(), Start) / 1000])       
    end.
    
collect_loop(Main) -> collect_loop_1(Main, dict:new(), undefined, 0).
collect_loop_1(Main, Dict, ChunkNum, ChunkNum) ->
    print_result(Dict),
    Main ! stop;
collect_loop_1(Main, Dict, ChunkNum, ProcessedNum) ->
    receive
        {chunk_num, ChunkNumX} -> 
            collect_loop_1(Main, Dict, ChunkNumX, ProcessedNum);
        {dict, DictX} ->
            Dict1 = dict:merge(fun (_, V1, V2) -> V1 + V2 end, Dict, DictX),
            collect_loop_1(Main, Dict1, ChunkNum, ProcessedNum + 1)
    end.

print_result(Dict) ->
    SortedList = lists:reverse(lists:keysort(2, dict:to_list(Dict))),
    [io:format("~b\t: ~s~n", [V, K]) || {K, V} <- lists:sublist(SortedList, 10)].
          
read_file(File, Collector) -> read_file(File, <<>>, 0, Collector).            
read_file(File, PrevTail, I, Collector) ->
    case file:read(File, ?BUFFER_SIZE) of
        eof -> 
            file:close(File),
            Collector ! {chunk_num, I};
        {ok, Bin} -> 
            {Data, NextTail} = split_on_last_newline(Bin),
            spawn(fun () -> Collector ! {dict, scan_chunk(<<PrevTail/binary, Data/binary>>)} end),
            read_file(File, NextTail, I + 1, Collector)
    end.

split_on_last_newline(Bin) -> split_on_last_newline(Bin, size(Bin)).   
split_on_last_newline(Bin, Offset) ->
    case Bin of
        <<Data:Offset/binary,$\n,Tail/binary>> ->
            {Data, Tail};
        _ when Offset =< 0 -> 
            {Bin, <<>>};
        _ -> 
            split_on_last_newline(Bin, Offset - 1)
    end.
    
scan_chunk(Bin) -> scan_chunk_1(Bin, 0, dict:new()).    
scan_chunk_1(Bin, Offset, Dict) when Offset < size(Bin) + 30 ->
    case Bin of
        <<_:Offset/binary,"GET /ongoing/When/",_,_,_,$x,$/,Y1,Y2,Y3,Y4,$/,M1,M2,$/,D1,D2,$/,Rest/binary>> ->            
            case match_until_space_newline(Rest, 0) of
                {Rest1, <<>>} -> 
                    scan_chunk_1(Rest1, 0, Dict);
                {Rest1, Word} -> 
                    Key = <<Y1,Y2,Y3,Y4,$/,M1,M2,$/,D1,D2,$/, Word/binary>>,
                    scan_chunk_1(Rest1, 0, dict:update_counter(Key, 1, Dict))
            end;
        _ -> scan_chunk_1(Bin, Offset + 1, Dict)
    end;
scan_chunk_1(_, _, Dict) -> Dict.

match_until_space_newline(Bin, Offset) ->
    case Bin of
        <<Word:Offset/binary,$ ,Rest/binary>> ->
            {Rest, Word};
        <<_:Offset/binary,$.,Rest/binary>> ->
            {Rest, <<>>};
        <<_:Offset/binary,10,Rest/binary>> ->
            {Rest, <<>>};
        <<_:Offset/binary,_,_/binary>> ->
            match_until_space_newline(Bin, Offset + 1);
        _ -> 
            {<<>>, <<>>}
    end. 
