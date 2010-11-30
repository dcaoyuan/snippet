-module(file_pread).

-compile([native]).

-export([start/2]).

-include_lib("kernel/include/file.hrl").

%% The best Buffer Size is 4096
-define(BUFFER_SIZE, 4096). 


start(FileName, ProcNum) ->
    [start(FileName, ProcNum, Fun) || Fun <- [fun read_file/3, fun pread_file/3]].

start(FileName, ProcNum, Fun) ->
    Start = now(),  

    Main = self(),
    Collector = spawn(fun () -> collect_loop(Main) end),

    Fun(FileName, ProcNum, Collector),
    
    %% don't terminate, wait here, until all tasks done.
    receive
        stop -> io:format("time: ~10.2f ms~n", [timer:now_diff(now(), Start) / 1000]) 
    end.

collect_loop(Main) -> collect_loop_1(Main, undefined, 0).
collect_loop_1(Main, ChunkNum, ChunkNum) -> 
    Main ! stop;
collect_loop_1(Main, ChunkNum, ProcessedNum) ->
    receive
        {chunk_num, ChunkNumX} ->
            collect_loop_1(Main, ChunkNumX, ProcessedNum);
        {seq, _Seq} ->
            collect_loop_1(Main, ChunkNum, ProcessedNum + 1)
    end.

get_chunk_size(FileName, ProcNum) ->
    {ok, #file_info{size=Size}} = file:read_file_info(FileName),
    Size div ProcNum.

read_file(FileName, ProcNum, Collector) ->
    ChunkSize = get_chunk_size(FileName, ProcNum),
    {ok, File} = file:open(FileName, [raw, binary]),
    read_file_1(File, ChunkSize, 0, Collector).
    
read_file_1(File, ChunkSize, I, Collector) ->
    case file:read(File, ChunkSize) of
        eof ->
            file:close(File),
            Collector ! {chunk_num, I};
        {ok, _Bin} -> 
            Collector ! {seq, I},
            read_file_1(File, ChunkSize, I + 1, Collector)
    end.

pread_file(FileName, ProcNum, Collector) ->
    ChunkSize = get_chunk_size(FileName, ProcNum),
    pread_file_1(FileName, ChunkSize, ProcNum, Collector).
       
pread_file_1(FileName, ChunkSize, ProcNum, Collector) ->
    [spawn(fun () ->
                   %% if it's last chuck, read all bytes left, 
                   %% which will not exceed ChunkSize * 2
                   Length = if  I == ProcNum - 1 -> ChunkSize * 2;
                                true -> ChunkSize end,
                   {ok, File} = file:open(FileName, [read, binary]),
                   {ok, _Bin} = file:pread(File, ChunkSize * I, Length),
                   Collector ! {seq, I},
                   file:close(File)
           end) || I <- lists:seq(0, ProcNum - 1)],
    Collector ! {chunk_num, ProcNum}.



    
pread_file_one(FileName, ProcNum, Collector) ->
    ChunkSize = get_chunk_size(FileName, ProcNum),
    {ok, File} = file:open(FileName, [read, binary]),
    pread_file_one_1(File, ChunkSize, ProcNum, Collector).
pread_file_one_1(File, ChunkSize, ProcNum, Collector) ->
    [spawn(fun () ->
                   %% if it's last chuck, read all bytes left, 
                   %% which will not exceed ChunkSize * 2
                   Length = if  I == ProcNum -> ChunkSize * 2;
                                true -> ChunkSize
                            end,
                   {ok, _Bin} = file:pread(File, ChunkSize * I, Length),
                   if  I == ProcNum - 1 -> file:close(File);
                       true -> go end,
                   Collector ! {seq, I}                
           end) || I <- lists:seq(0, ProcNum - 1)],
    Collector ! {chunk_num, ProcNum}.   
    