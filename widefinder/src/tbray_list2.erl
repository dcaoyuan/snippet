-module(tbray_list2).

-compile([native, {hipe,  [o3]}]).

-export([start/1, start/2]).

-record(context, {main,                 
                  dict,                 
                  chunkNum,                 
                  processedNum = 0}).

start(FileName) -> start(FileName, 4096).

start(FileName, ChunkSize) ->   
    Start = now(),   
    Main = self(),   
    Collector = spawn_link(fun () -> collect_loop(#context{main = Main,                                                     
                                                           dict = dict:new(),                                                     
                                                           processedNum = 0}) end),   
    
    ChunkNum = foreach_chunk(           
      fun (Chunk) ->               
              spawn_link(fun() -> Collector ! scan_lines(Chunk) end)           
      end, FileName, ChunkSize),   
    
    Collector ! {chunkNum, ChunkNum},
    
    %% don't terminate, wait here, until all tasks done.   
    receive       
        stop -> io:format("Time: ~p ms~n", [timer:now_diff(now(), Start) / 1000])   
    end.
        
foreach_chunk(Fun, FileName, SizeOfChunk) ->   
    {ok, File} = file:open(FileName, [raw, binary]),   
    N = foreach_chunk(Fun, File, [], SizeOfChunk, 0),   
    file:close(File),   
    N.
    
foreach_chunk(Fun, File, PrevRest, SizeOfChunk, N) ->   
    {Chunk, Rest} = read_chunk(File, PrevRest, SizeOfChunk),   
    Fun(Chunk),
    case Rest of
        [] -> N + 1;
        _ -> 
            foreach_chunk(Fun, File, Rest, SizeOfChunk, N + 1)   
    end.
    
read_chunk(File, PrevRest, SizeOfChunk) ->   
    case file:read(File, SizeOfChunk) of       
        {ok, Bin} ->           
            {Line, NextRest} = split_on_nl(binary_to_list(Bin)),
            Chunk = PrevRest ++ Line,           
            case NextRest of               
                [] ->                   
                    read_chunk(File, Chunk, SizeOfChunk);             
                _ ->                   
                    {Chunk, NextRest}          
            end;
        eof ->           
            {PrevRest, []}
    end.
    
split_on_nl(List) -> split_on_nl(List, []).
split_on_nl(List, Line) -> 
    case List of
        [] -> {List, []};
        [$\n|Rest] ->
            %io:format("~nLine:~p~n", [Line]),
            {lists:reverse(Line), Rest};
        [C|Rest] -> 
            split_on_nl(Rest, [C | Line])
    end.

collect_loop(#context{main=Main,                     
                      dict=Dict,                     
                      chunkNum=ChunkNum,                     
                      processedNum=ProcessedNum}=Context) ->   
    case ProcessedNum of       
        ChunkNum ->           
            print_result(Dict),           
            Main ! stop;
        _ ->           
            receive               
                {chunkNum, N} -> 
                    collect_loop(Context#context{chunkNum = N});               
                DictX ->                   
                    collect_loop(Context#context{dict = dict:merge(fun (_, V1, V2) -> 
                                                                           V1 + V2 
                                                                   end, Dict, DictX),                       
                                                 processedNum = ProcessedNum+1})          
                                                 
            end   
    end.
    
print_result(Dict) ->   
    [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10 | _] = 
      lists:reverse(lists:keysort(2, dict:to_list(Dict))),   
    lists:foreach(fun ({Word, Count}) ->                         
                          io:format("~p get requests for ~s~n", [Count, Word])                 
                  end, [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10]).
                      
scan_lines(List) -> scan_lines(List, dict:new()).
scan_lines(List, Dict) ->
    case naive_search(List) of
        {ok, Key, Rest} ->        
            scan_lines(
              element(2, split_on_nl(Rest)),               
              dict:update_counter(Key, 1, Dict));       
        false -> Dict   
    end.
    
%% naive_search(Binary()) -> false | {ok, Key, Rest}
naive_search(List) ->   
    case List of
        [] -> false;
        "GET /ongoing/When/"++Rest -> 
            case keyMatch(Rest) of               
                Result = {ok, _Key, _Rest2} -> Result;
                false -> naive_search(Rest)           
            end;
        [_|Rest] -> naive_search(Rest)
    end.
    
%% keyMatch(Binary()) -> false | {ok, Key, Rest}
keyMatch([C|_]) when C == $ ; C == $. -> false;
keyMatch(List) -> keyMatch(List, []).

keyMatch(List, Word) ->   
    case List of
        [] -> false;
        [$ |Rest] ->
            {ok, lists:reverse(Word), Rest};       
        [$.|_] -> 
            false;
        [C|Rest] -> 
            keyMatch(Rest, [C | Word])
    end.
    
