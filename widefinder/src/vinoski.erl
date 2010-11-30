-module(vinoski).

-export([start/2, main/1]).
-compile([native]).

main([N, F]) ->
    io:format("~p matches found~n", [start(list_to_integer(N), F)]),
    halt();
main([F]) ->
    Sz = [16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384],
    Results = lists:map(fun(S) ->
                                Start = now(),
                                start(S, F, S * 1024),
                                {S, time_diff(Start, now())}
                        end, Sz),
    io:format("~p~n", [Results]),
    halt().
    
start(Num, File) -> start(Num, File, 512 * 1024).
start(Num, File, ChunkSize) ->
    bfile:load_driver(),
    {ok, F} = bfile:fopen(File, "r"),
    Pids = scan_file(F, Num, ChunkSize),
    bfile:fclose(F),
    lists:foldr(fun(_, T) -> receive V -> T + V end end, 0, Pids).

scan_file(F, N, ChunkSize) -> scan_file(F, N, ChunkSize, self(), <<>>, []).
scan_file(F, N, ChunkSize, Me, Leftover, Pids) ->
    case bfile:fread(F, ChunkSize) of
        eof -> scan_finish(Leftover, Me, Pids);
        {ok, Bin} ->
            {Bins, More} = break_chunk_on_newline(list_to_binary([Leftover, Bin]), N),
            scan_file(F, N, ChunkSize, Me, More, [spawn_collector(Bins, Me) | Pids])
    end.    

scan_finish(<<>>, _, Pids) -> Pids;
scan_finish(More, Me, Pids) -> [spawn_collector([More], Me) | Pids].

spawn_collector(Bins, Me) ->
    Collector = spawn(
      fun() -> 
              V = lists:foldr(fun(_, T) -> 
                                      receive V -> T + V end 
                              end, 0, Bins),
              Me ! V
      end),
    [process_binary(Collector, Bin) || Bin <- Bins],
    Collector.
    
break_chunk_on_newline(Bin, N) -> break_chunk_on_newline(Bin, size(Bin) div N, []).
break_chunk_on_newline(Bin, Pos, All) when Pos >= size(Bin) -> {All, Bin};
break_chunk_on_newline(Bin, Pos, All) ->
    <<_:Pos/binary,C:8,_/binary>> = Bin,
    case C of
        $\n ->
            L = Pos + 1,
            <<Line:L/binary,Rest/binary>> = Bin,
            break_chunk_on_newline(Rest, Pos, All ++ [Line]);
        _ -> 
            break_chunk_on_newline(Bin, Pos + 1, All)
    end.

process_binary(Pid, Bin) ->
    spawn(
      fun() ->
              L = string:tokens(binary_to_list(Bin), "n"),
              V = lists:foldr(fun(Line, Total) ->
                                      Tok = string:tokens(Line, " "),
                                      Total + find_match(lists:nth(7, Tok))
                              end, 0, L),
              Pid ! V
      end).
      
find_match("/ongoing/When/" ++ Last) ->
    case lists:member($., Last) of
        false -> 1;
        true -> 0
    end;
find_match(_) -> 0.    


