-module(tbray_pichi).

-compile([native, {hipe,  [o3]}]).

-export([start/1, start/2]).

-record(context, {main,                 
                  dict,                 
                  chunkNum,                 
                  processedNum = 0}).

start(FileName) -> start(FileName, 1024*32).

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
    N = foreach_chunk(Fun, File, <<>>, SizeOfChunk, 0),   
    file:close(File),   
    N.
foreach_chunk(Fun, File, PrevRest, SizeOfChunk, N) ->   
    {Chunk, Rest} = read_chunk(File, PrevRest, SizeOfChunk),   
    Fun(Chunk),   
    case Rest of       
        <<>> -> N + 1;       
        _ -> 
            foreach_chunk(Fun, File, Rest, SizeOfChunk, N+1)   
    end.
    
read_chunk(File, PrevRest, N) ->   
    case file:read(File, N) of       
        {ok, B} ->           
            {Line, Rest} = split_on_nl(B),           
            Chunk = <<PrevRest/binary, Line/binary>>,           
            case Rest of               
                <<>> ->                   
                    read_chunk(File, Chunk, N);               
                _ ->                   
                    {Chunk, Rest}           
            end;
        eof ->           
            {PrevRest, <<>>}   
    end.
    
split_on_nl(B) -> split_on_nl(B, 0, size(B)).
split_on_nl(B, N, S) when N < S -> 
    case B of
        <<Line:N/binary, $\n, Tail/binary>> -> 
            {Line, Tail};
        _ -> 
            split_on_nl(B, N + 1, S)   
    end;
split_on_nl(B, _, _) -> {B, <<>>}.

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
                {chunkNum, N} -> collect_loop(Context#context{chunkNum = N});               
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
                      
scan_lines(Bin) -> scan_lines(Bin, dict:new()).
scan_lines(Bin, Dict) ->   
    case naive_search(Bin) of
        {ok, Key, Rest} ->        
            scan_lines(
              element(2, split_on_nl(Rest)),               
              dict:update_counter(Key, 1, Dict));       
        false -> Dict   
    end.
    
%% naive_search(Binary()) -> false | {ok, Key, Rest}
naive_search(B) -> naive_search(B, 0, size(B) - 18).
naive_search(B, N, S) when N < S ->   
    case B of
        <<_:N/binary, "GET /ongoing/When/", Rest/binary>> -> 
            case keyMatch(Rest) of               
                Result = {ok, _Key, _Rest2} -> Result;
                false -> naive_search(Rest)           
            end;
        _ -> naive_search(B, N + 1, S)   
    end;
naive_search(_, _, _) -> false.
    
%% keyMatch(Binary()) -> false | {ok, Key, Rest}
keyMatch(<<C, _/binary>>) when C == $ ; C == $. -> false;   % empty
keyMatch(B) -> keyMatch(B, 1, size(B)).

keyMatch(B, N, S) when N < S ->   
    case B of
        % end with space       
        <<Key:N/binary, $ , Rest/binary>> -> 
            {ok, Key, Rest};       
        <<_:N/binary, $., _/binary>> -> 
            false;
        _ -> 
            keyMatch(B, N + 1, S)   
    end;
keyMatch(_, _, _) -> false.
    
