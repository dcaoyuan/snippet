-module(tbray_pread).

-compile([native]).

-export([start/2,
         counter_loop/2,
         collect_loop/1,
         buffered_read/3,
         process_match/1]).

-export([split_on_newline/1,
         split_on_char/2,
         test_split_on_newline/1]).

-include_lib("kernel/include/file.hrl").

%% The best Buffer Size is 4096
-define(BUFFER_SIZE, 4096).

-record(context, {counter,
                  lastSeq = 0,
                  dataBuf = [],
                  chunkTails, % {list(), list()...} 
                  processNum}).

%% erl -smp enable +P 60000
%% timer:tc(tbray, start, ["o1000k.ap", 100]).   
start(FileName, ProcessNum) ->
    statistics(wall_clock),  
    {ok, FileInfo} = file:read_file_info(FileName), 
    Size = FileInfo#file_info.size,
    
    % we need ProcessNum + 1 space to store last chunk's tail
    ChunkTails = list_to_tuple(lists:duplicate(ProcessNum + 1, [])),
    Main = self(),
    Counter = spawn(fun () -> counter_loop(Main, dict:new()) end),
    Collect = spawn(fun () -> collect_loop(#context{processNum = ProcessNum,
                                                    chunkTails = ChunkTails,
                                                    counter = Counter}) end),


    
    pread_file(Collect, FileName, Size div ProcessNum, ProcessNum, 1),
    %{ok, File} = file:open(FileName, [raw, binary]),
    %read_file(Collect, File, Size div ProcessNum, 1),
    
    %% don't terminate, wait here, until all tasks done.
    receive
        stop -> 
            {_, Duration2} = statistics(wall_clock),
            io:format("Took: ~B ms~n", [Duration2]) 
    end.

read_file(Collector, File, ChunkSize, I) ->
    case file:read(File, ChunkSize) of
        eof -> 
            file:close(File),
            Collector ! stop;
        {ok, Data} -> 
            Collector ! {I, Data},
            read_file(Collector, File, ChunkSize, I + 1)
    end.

pread_file(_, _, _, ProcessNum, I) when I > ProcessNum -> done;
pread_file(Collector, FileName, ChunkSize, ProcessNum, I) ->
    spawn(fun () ->
                  Offset = ChunkSize * (I - 1),
                  %% if it's last chuck, read all bytes left, 
                  %% which will not exceed ChunkSize * 2
                  Length = if  I == ProcessNum -> ChunkSize * 2;
                               true -> ChunkSize
                           end,
                  {ok, File} = file:open(FileName, [read, binary]),
                  {ok, Data} = file:pread(File, Offset, Length),
                  file:close(File),
                  Collector ! {I, Data}
          end),
    pread_file(Collector, FileName, ChunkSize, ProcessNum, I + 1).


collect_loop(#context{counter=Counter,
                      lastSeq=ProcessNum,
                      processNum=ProcessNum}) -> Counter ! stop;
collect_loop(#context{dataBuf=DataBuf}=Context) ->
    receive
        stop -> io:format("Reading file done.~n");
        {Seq, Data} ->
            SortedDatas = lists:keysort(1, [{Seq, Data} | DataBuf]),
            Context1 = process_datas(SortedDatas, Context#context{dataBuf = []}),
            collect_loop(Context1)
    end.

process_datas([], Context) -> Context;
process_datas([{Seq, Data}|Rest], #context{counter=Counter,
                                           lastSeq=LastSeq, 
                                           dataBuf=DataBuf,
                                           chunkTails=ChunkTails}=Context) ->
    if  Seq == LastSeq + 1 ->
            PrevTail = element(Seq, ChunkTails),
            {NextTail, Dict} = 
              buffered_read(fun (Buf, {LineBufX, DictX}) ->
                                    get_and_scan_lines(binary_to_list(Buf), LineBufX, DictX)
                            end, {PrevTail, dict:new()}, Data),
            Counter ! Dict,
            ChunkTails1 = setelement(Seq, ChunkTails, []),
            ChunkTails2 = setelement(Seq + 1, ChunkTails1, NextTail),
            % todo: prcoess last chunk's tail
            process_datas(Rest, Context#context{chunkTails = ChunkTails2,
                                                lastSeq = Seq});
        true -> 
            process_datas(Rest, Context#context{dataBuf = [{Seq, Data} | DataBuf]})
    end.

counter_loop(Pid, Dict) ->
    receive
        stop -> 
            print_result(Dict),
            Pid ! stop;
        DictX -> 
            Dict1 = dict:merge(fun(_, V1, V2) -> V1 + V2 end, Dict, DictX),
            counter_loop(Pid, Dict1)
    end.

print_result(Dict) ->
    SortedList = lists:reverse(lists:keysort(2, dict:to_list(Dict))),
    [io:format("~b\t: ~s~n", [V, K]) || {K, V} <- lists:sublist(SortedList, 10)].

buffered_read(Fun, Acc, Bin) ->
    case Bin of
        <<Buf:?BUFFER_SIZE/binary, Rest/binary>> ->
            Acc1 = Fun(Buf, Acc),
            buffered_read(Fun, Acc1, Rest);
        _ ->
            Fun(Bin, Acc)
    end.

get_and_scan_lines(List, LineBuf, Dict) ->
    case List of
        [] -> {LineBuf, Dict};
        [$\n|Rest] -> 
            Line = lists:reverse(LineBuf),
            %io:format("~n~s~n", [Line]),
            Dict1 = case process_match(Line) of
                        {true, Word} -> 
                            dict:update_counter(Word, 1, Dict);
                        false ->
                            Dict 
                    end,
            get_and_scan_lines(Rest, [], Dict1);
        [C|Rest] ->
            get_and_scan_lines(Rest, [C | LineBuf], Dict)
    end.

process_match(Line) ->
    case Line of
        [] -> false;
        "/ongoing/When/"++Rest -> 
            match_until_space(Rest, []);
        [_|Rest] -> 
            process_match(Rest)
    end.
    
match_until_space(List, Acc) ->
    case List of
        [] -> false;
        [$ |_] -> {true, lists:reverse(Acc)};
        [$.|_] -> false;
        [C|Rest] -> match_until_space(Rest, [C | Acc])
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


test1(FileName) ->
    statistics(wall_clock),  
    {ok, Bin} = file:read_file(FileName),
    {Matched, Total} = scan_line1(Bin),
    {_, Duration} = statistics(wall_clock),
    io:format("Duration ~pms~n Matched:~B, Total:~B", [Duration, Matched, Total]).    

scan_line1(Bin) -> scan_line1(Bin, [], 0, 0).
scan_line1(<<>>, _Line, Matched, Total) -> {Matched, Total};
scan_line1(<<$\n, Rest/binary>>, Line, Matched, Total) -> 
    Line1 = lists:reverse(Line),
    scan_line1(Rest, [], Matched, Total + 1);
scan_line1(<<C, Rest/binary>>, Line, Matched, Total) ->
    %NewCount = Matched + process_match(Line),
    scan_line1(Rest, [C | Line], Matched, Total).


test_split_on_newline(Bin) ->
    statistics(wall_clock),
    
    split_on_char(Bin, $\n),
    io:format("bin split without 10 build-in: ~p~n", [statistics(wall_clock)]),

    split_on_newline(Bin),
    io:format("bin split with 10 build-in: ~p~n", [statistics(wall_clock)]).    

