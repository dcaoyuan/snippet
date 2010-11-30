-module(tbray_blog1).

-compile([native]).

-export([start/2,
         collect_loop/2,
         buffered_read/3]).

-include_lib("kernel/include/file.hrl").

%% The best BUFFER_SIZE is 4096
-define(BUFFER_SIZE, 4096).

-record(context, {lineBuf = [], 
                  matched = 0,
                  total = 0,
                  lastProcessedSeq = 0,
                  dataBuf = [],
                  processNum}).

%% erl -smp enable +P 60000
%% timer:tc(wide, start, [1000, "o1000k.ap"]).   
start(ProcessNum, FileName) ->
    statistics(wall_clock),  
    {ok, FileInfo} = file:read_file_info(FileName), 
    Size = FileInfo#file_info.size,
    
    Collect = spawn(?MODULE, collect_loop, [self(), #context{processNum = ProcessNum}]),

    psplit_read_file(Collect, FileName, Size div ProcessNum, ProcessNum, 1),
    
    {Matched, Total} =
      receive
          #context{matched=MatchedX, total=TotalX} -> {MatchedX, TotalX}
      end,

    {_, Duration2} = statistics(wall_clock),
    io:format("scan lines:\t ~pms~nMatched: ~B, Total: ~B~n", [Duration2, Matched, Total]). 

psplit_read_file(_Collector, _FileName, _ChunkSize, ProcessNum, I) when I > ProcessNum -> done;
psplit_read_file(Collector, FileName, ChunkSize, ProcessNum, I) ->
    spawn(
      fun () ->
              Offset = ChunkSize * (I - 1),
              %% if it's last chuck, read all bytes left, which will not exceed ChunkSize * 2
              Length = if  I == ProcessNum -> ChunkSize * 2;
                           true -> ChunkSize
                       end,
              {ok, File} = file:open(FileName, [read, binary]),         
              {ok, Data} = file:pread(File, Offset, Length),
              Collector ! {I, Data}
      end),
    psplit_read_file(Collector, FileName, ChunkSize, ProcessNum, I + 1).


collect_loop(Pid, #context{lastProcessedSeq= ProcessNum,
                           processNum=ProcessNum}=Context) -> Pid ! Context;
collect_loop(Pid, #context{dataBuf=DataBuf}=Context) ->
    receive
        {Seq, Data} ->
            SortedDatas = lists:keysort(1, [{Seq, Data} | DataBuf]),
            Context1 = process_arrived_datas(SortedDatas, Context#context{dataBuf = []}),
            collect_loop(Pid, Context1)
    end.

process_arrived_datas([], Context) -> Context;
process_arrived_datas([{Seq, Data}|T], #context{lineBuf=LineBuf,
                                                matched=Matched,
                                                total=Total,
                                                lastProcessedSeq=LastProcessedSeq, 
                                                dataBuf=DataBuf}=Context) ->
    if  Seq == LastProcessedSeq + 1 ->
            {LineBuf1, Matched1, Total1} = buffered_read(
              fun (Buffer, {LineBufX, MatchedX, TotalX}) ->
                      scan_line(binary_to_list(Buffer), LineBufX, MatchedX, TotalX)
              end, {LineBuf, Matched, Total}, Data),
            process_arrived_datas(T, Context#context{lineBuf = LineBuf1,
                                                     matched = Matched1, 
                                                     total = Total1,
                                                     lastProcessedSeq = Seq});
        true -> 
            process_arrived_datas(T, Context#context{dataBuf = [{Seq, Data} | DataBuf]})
    end.

buffered_read(Fun, Acc, Bin) ->
    case Bin of
        <<Buf:?BUFFER_SIZE/binary, Rest/binary>> ->
            Acc1 = Fun(Buf, Acc),
            buffered_read(Fun, Acc1, Rest);
        _ ->
            Fun(Bin, Acc)
    end.
    
scan_line([], LineBuf, Matched, Total) -> {LineBuf, Matched, Total};
scan_line([$\n|Rest], LineBuf, Matched, Total) -> 
    Line1 = lists:reverse(LineBuf),
    %io:format("~n~s~n", [Line1]),
    Matched1 = Matched + process_match(Line1),
    scan_line(Rest, [], Matched1, Total + 1);
scan_line([C|Rest], LineBuf, Matched, Total) ->
    scan_line(Rest, [C | LineBuf], Matched, Total).

process_match([]) -> 0;
process_match("GET /ongoing/When/"++Rest) ->
    case match_until_space(Rest, false) of
	true  -> 0;
	false -> 1
    end;
process_match([_H|Rest]) -> 
    process_match(Rest).
    
match_until_space([$\040|_Rest], Bool) -> Bool;
match_until_space([$.|_Rest], _Bool) -> true;
match_until_space([_H|Rest], Bool) -> 
    match_until_space(Rest, Bool).