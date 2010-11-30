-module(tbray1).

-compile([native]).

-export([start/1]).

start(FileName) ->
    Start = now(),

    Main = self(),
    {ok, File} = file:open(FileName, [read, binary]),    
    read_file(File, Main),

    %% don't terminate, wait here, until all tasks done.
    receive
        stop -> io:format("Time: ~10.2f ms~n", [timer:now_diff(now(), Start) / 1000])       
    end.

print_result(Dict) ->
    SortedList = lists:reverse(lists:keysort(2, dict:to_list(Dict))),
    [io:format("~b\t: ~p~n", [V, K]) || {K, V} <- lists:sublist(SortedList, 10)].
          
read_file(File, Main) -> read_file_1(File, dict:new(), Main).            
read_file_1(File, Dict, Main) ->
    case io:get_line(File, '') of
        eof -> 
            file:close(File),
            print_result(Dict),
            Main ! stop;
        Bin ->
            read_file_1(File, scan_chunk(Bin, Dict), Main)
    end.
    
scan_chunk(Bin, Dict) -> scan_chunk_1(Bin, 0, Dict).
scan_chunk_1(Bin, Offset, Dict) when Offset < size(Bin) - 34 ->
    case Bin of
        <<_:Offset/binary,"GET /ongoing/When/",_,_,_,$x,$/,_,_,_,_,$/,_,_,$/,_,_,$/,_/binary>> ->            
            S = Offset + 34,
            case match_until_space_newline(Bin, S) of
                false -> 
                    Dict;
                {true, E} when S == E ->
                    Dict;
                {true, E} ->
                    L = E - S,
                    <<_:S/binary,Key:L/binary,_/binary>> = Bin,
                    dict:update_counter(Key, 1, Dict)
            end;
        _ -> scan_chunk_1(Bin, Offset + 1, Dict)
    end;
scan_chunk_1(_, _, Dict) -> Dict.

match_until_space_newline(Bin, Offset) when Offset < size(Bin) ->
    case Bin of
        <<_:Offset/binary,$ ,_/binary>> ->
            {true, Offset};
        <<_:Offset/binary,$.,_/binary>> ->
            false;
        <<_:Offset/binary,10,_/binary>> ->
            false;
        _ ->
            match_until_space_newline(Bin, Offset + 1)
    end;
match_until_space_newline(_, _) -> false.


