-module(tbray_bin).

-compile([native]).

-export([start/1]).

-export([split_on_newline/1, 
         split_on_char/2,
         test_split_on_newline/1]).

-define(BUFFER_SIZE, (4096 * 1000)).


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
    [io:format("~b\t: ~p~n", [V, K]) || {K, V} <- lists:sublist(SortedList, 10)].
          
read_file(File, Collector) -> read_file(File, <<>>, 0, Collector).            
read_file(File, PrevTail, I, Collector) ->
    case file:read(File, ?BUFFER_SIZE) of
        eof -> 
            file:close(File),
            Collector ! {chunk_num, I};
        {ok, Bin} -> 
            {Data, NextTail} = split_on_last_newline(Bin),
            spawn(fun () -> Collector ! {dict, scan_lines(<<PrevTail/binary, Data/binary>>)} end),
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
    
scan_lines(Bin) -> scan_lines_1(Bin, 0, dict:new()).    
scan_lines_1(Bin, Offset, Dict) ->
    case Bin of
        <<Line:Offset/binary,$\n,Rest/binary>> ->
            scan_lines_1(Rest, 0, match_and_update_dict(Line, Dict));
        <<_:Offset/binary,_,_/binary>> ->
            scan_lines_1(Bin, Offset + 1, Dict);
        _ -> match_and_update_dict(Bin, Dict)
    end.

match_and_update_dict(Line, Dict) ->
    case process_match(Line) of
        false -> Dict;
        {true, Word} -> 
            dict:update_counter(Word, 1, Dict)
    end.

process_match(Bin) -> process_match(Bin, 0, size(Bin) - 18).
process_match(Bin, Offset, Size) when Offset < Size ->
    case Bin of
        <<_:Offset/binary,"GET /ongoing/When/",Rest/binary>> ->
            match_until_space(Rest, 0); 
        _ -> process_match(Bin, Offset + 1, Size)
    end;
process_match(_, _, _) -> false.

match_until_space(Bin, Offset) ->
    case Bin of
        <<Word:Offset/binary,$ ,_/binary>> ->
            {true, Word};
        <<_:Offset/binary,$.,_/binary>> ->
            false;
        <<_:Offset/binary,_,_/binary>> ->
            match_until_space(Bin, Offset + 1);
        _ -> false
    end. 


split_on_newline(Bin) ->
    split_on_newline(Bin, 0, []).

split_on_newline(Bin, S, Acc) ->
    %% Compiler will build a decision tree for following binary pattern matchs 
    %% and optimaze it, so, knows how many bits will be traveled and tested.
    case Bin of
        <<_:S/binary,10,_/binary>> ->
            L = S,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_newline(Rest, 0, [New | Acc]);
        <<_:S/binary,_,10,_/binary>> ->
            L = S + 1,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_newline(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,10,_/binary>> ->
            L = S + 2,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_newline(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,10,_/binary>> ->
            L = S + 3,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_newline(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,10,_/binary>> ->
            L = S + 4,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_newline(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,10,_/binary>> ->
            L = S + 5,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_newline(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,_,10,_/binary>> ->
            L = S + 6,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_newline(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,_,_,10,_/binary>> ->
            L = S + 7,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_newline(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,_,_,_,10,_/binary>> ->
            L = S + 8,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_newline(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,_,_,_,_,10,_/binary>> ->
            L = S + 9,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_newline(Rest, 0, [New | Acc]);
        <<_:S/binary,_:80,_/binary>> ->
            split_on_newline(Bin, S + 10, Acc);
        <<_:S/binary,_,_/binary>> ->
            split_on_newline(Bin, S + 1, Acc);
        _ ->
            {Bin, lists:reverse(Acc)}
    end.
    
%% @doc split Bin on character C
%% -spec(split_on_char/2::Bin::binary(), C:char() -> [binary]). 
split_on_char(Bin, C) ->
    split_on_char(Bin, C, 0, []).

split_on_char(Bin, C, S, Acc) ->
    %% Compiler will build a decision tree for following binary pattern matchs 
    %% and optimaze it, so, knows how many bits will be traveled and tested.
    case Bin of
        <<_:S/binary,C,_/binary>> ->
            L = S,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_char(Rest, C, 0, [New | Acc]);
        <<_:S/binary,_,C,_/binary>> ->
            L = S + 1,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_char(Rest, C, 0, [New | Acc]);
        <<_:S/binary,_,_,C,_/binary>> ->
            L = S + 2,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_char(Rest, C, 0, [New | Acc]);
        <<_:S/binary,_,_,_,C,_/binary>> ->
            L = S + 3,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_char(Rest, C, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,C,_/binary>> ->
            L = S + 4,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_char(Rest, C, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,C,_/binary>> ->
            L = S + 5,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_char(Rest, C, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,_,C,_/binary>> ->
            L = S + 6,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_char(Rest, C, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,_,_,C,_/binary>> ->
            L = S + 7,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_char(Rest, C, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,_,_,_,C,_/binary>> ->
            L = S + 8,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_char(Rest, C, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,_,_,_,_,C,_/binary>> ->
            L = S + 9,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_on_char(Rest, C, 0, [New | Acc]);
        <<_:S/binary,_:80,_/binary>> ->
            split_on_char(Bin, C, S + 10, Acc);
        <<_:S/binary,_,_/binary>> ->
            split_on_char(Bin, C, S + 1, Acc);
        _ ->
            {Bin, lists:reverse(Acc)}
    end.

scan_lines_advanced(Counter, Bin, Offset, LastOffset) ->
    case Bin of
        <<_:Offset/binary,$\n,_/binary>> ->
            Length = Offset - LastOffset,
            <<_:LastOffset/binary,Line:Length/binary,_/binary>> = Bin,
            %io:format("~n~p~n", [Line]),
            spawn(fun () -> process_match(Line) end),
            scan_lines_advanced(Counter, Bin, Offset + 1, Offset + 1);
        <<_:Offset/binary,_,_/binary>> ->
            scan_lines_advanced(Counter, Bin, Offset + 1, LastOffset);
        _ -> 
            <<_:LastOffset/binary,Rest/binary>> = Bin,
            Rest
    end.

%% ======= Another solution that read dataset to bin first. ========
    
start_pbin(ProcessNum, FileName) ->
    statistics(wall_clock),  
    {ok, Bin} = file:read_file(FileName),
    {_, Duration1} = statistics(wall_clock),
    io:format("Read file\t ~pms~n", [Duration1]),

    Scanner = spawn(?MODULE, scanner_loop, [self(), [], 0, 0]),
    Collect = spawn(?MODULE, collect_loop, [Scanner, [], 0, ProcessNum]),

    psplit_bin(Collect, Bin, ProcessNum),
    
    {Matched, Total} =
      receive
          {MatchedX, TotalX} -> {MatchedX, TotalX}
      end,

    {_, Duration2} = statistics(wall_clock),
    io:format("scan lines:\t ~pms~nMatched: ~B, Total: ~B~n", [Duration2, Matched, Total]).   

psplit_bin(Collector, Bin, ProcessNum) ->
    Size = size(Bin),
    ChunkSize = Size div ProcessNum,
    %io:format("Num: ~B, ChunkSize: ~B~n", [ProcessNum, ChunkSize]),
    psplit_bin_1(Collector, Bin, ChunkSize, ProcessNum, 1).
    
psplit_bin_1(_Collector, _Bin, _ChunkSize, ProcessNum, Seq) when Seq > ProcessNum -> done;
psplit_bin_1(Collector, Bin, ChunkSize, ProcessNum, Seq) ->
    Offset = ChunkSize * (Seq - 1),
    spawn(
      fun () ->
              %io:format("Seq: ~B, Offset: ~B, Offset + ChunkSize: ~B~n", [Seq, Offset, Offset + ChunkSize]),
              Data = if  Seq == ProcessNum ->
                             %<<_Skipped:Offset/binary, DataX/binary>> = Bin,
                             {_, DataX} = split_binary(Bin, Offset),
                             DataX;
                         true ->
                             %<<_Skipped:Offset/binary, DataX:ChunkSize/binary, _RestX/binary>> = Bin,
                             {_, RestX} = split_binary(Bin, Offset),
                             {DataX, _} = split_binary(RestX, ChunkSize),
                             DataX
                     end,
              %io:format("Seq send: ~B, Num: ~B~n", [Seq, ProcessNum]),
              Collector ! {Seq, Data}
      end),
    psplit_bin_1(Collector, Bin, ChunkSize, ProcessNum, Seq + 1).



test_split_on_newline(Bin) ->
    statistics(wall_clock),
    
    split_on_char(Bin, $\n),
    io:format("bin split without 10 build-in: ~p~n", [statistics(wall_clock)]),

    split_on_newline(Bin),
    io:format("bin split with 10 build-in: ~p~n", [statistics(wall_clock)]).    


