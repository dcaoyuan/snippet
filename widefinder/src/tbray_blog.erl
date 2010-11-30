-module(tbray_blog).

-compile([native]).

-export([start/1]).

%% The best Bin Buffer Size is 4096
-define(BUFFER_SIZE, 4096). 

start(FileName) ->
    Start = now(),

    Main = self(),
    Collector = spawn(fun () -> collect_loop(Main) end),

    {ok, File} = file:open(FileName, [raw, binary]),
    read_file(File, Collector),
    
    %% don't terminate, wait here, until all tasks done.
    receive
        stop -> io:format("Time: ~10.2f ms~n", [timer:now_diff(now(), Start) / 1000])
    end.

read_file(File, Collector) -> read_file_1(File, [], 0, Collector).
read_file_1(File, PrevTail, I, Collector) ->
    case file:read(File, ?BUFFER_SIZE) of
        eof ->
            Collector ! {chunk_num, I},
            file:close(File);
        {ok, Bin} -> 
            {Data, NextTail} = split_on_last_newline(PrevTail ++ binary_to_list(Bin)),
            spawn(fun () -> Collector ! {dict, scan_lines(Data)} end),
            read_file_1(File, NextTail, I + 1, Collector)
    end.

split_on_last_newline(List) -> split_on_last_newline_1(lists:reverse(List), []).
split_on_last_newline_1(List, Tail) ->
    case List of
        []         -> {lists:reverse(List), []};
        [$\n|Rest] -> {lists:reverse(Rest), Tail};
        [C|Rest]   -> split_on_last_newline_1(Rest, [C | Tail])
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
    [io:format("~p\t: ~s~n", [V, K]) || {K, V} <- lists:sublist(SortedList, 10)].

scan_lines(List) -> scan_lines_1(List, [], dict:new()).
scan_lines_1(List, Line, Dict) -> 
    case List of
        [] -> match_and_update_dict(lists:reverse(Line), Dict);
        [$\n|Rest] ->
            scan_lines_1(Rest, [], match_and_update_dict(lists:reverse(Line), Dict));
        [C|Rest] ->
            scan_lines_1(Rest, [C | Line], Dict)
    end.

match_and_update_dict(Line, Dict) ->
    case process_match(Line) of
        false -> Dict;
        {true, Word} -> 
            dict:update_counter(Word, 1, Dict)
    end.
    
process_match(Line) ->
    case Line of
        [] -> false;
        "GET /ongoing/When/"++[_,_,_,$x,$/,Y1,Y2,Y3,Y4,$/,M1,M2,$/,D1,D2,$/|Rest] -> 
            case match_until_space(Rest, []) of
                [] -> false;
                Word -> {true, [Y1,Y2,Y3,Y4,$/,M1,M2,$/,D1,D2,$/] ++ Word}
            end;
        [_|Rest] -> 
            process_match(Rest)
    end.
    
match_until_space(List, Word) ->
    case List of
        [] -> [];
        [$.|_] -> [];
        [$ |_] -> lists:reverse(Word);
        [C|Rest] -> match_until_space(Rest, [C | Word])
    end.
