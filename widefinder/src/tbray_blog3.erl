-module(tbray_blog3).

-compile([native]).

-export([start/1]).
  
%% The best Bin Buffer Size is 4096 * 1 - 4096 * 5
-define(BUFFER_SIZE, (4096 * 5)). 

%% ==== When BUFFER_SIZE = 4096 * 5: ====
%% $ time erl -smp +P 60000 -noshell -run tbray_list_no_line start o1000k.ap -s erlang halt > /dev/null
%%
%% real    0m4.596s
%% user    0m5.907s
%% sys     0m0.631s
%% 
%% without split_on_last_newline:
%% real    0m4.370s
%% user    0m4.610s
%% sys     0m1.694s
%% 
%% so, split_on_last_newline took about 4.596 - 4.370 = 0.226s 
%%
%% ==== When BUFFER_SIZE = 4096 ====
%% $ erlc -smp tbray.erl
%% $ time erl -smp +P 60000 -noshell -run tbray start o1000k.ap -s erlang halt > /dev/null
%% 
%% real    0m6.352s
%% user    0m7.985s
%% sys     0m1.090s
%%
%% without split_on_last_newline:
%% real    0m3.790s
%% user    0m5.076s
%% sys     0m0.781s
%%
%% so, split_on_last_newline took about 6.352 - 3.790 = 2.56s

%% $ erlc tbray.erl
%% $ time erl -noshell -run tbray start o1000k.ap -s erlang halt > /dev/null
%% 
%% real    0m8.140s
%% user    0m7.117s
%% sys     0m0.773s
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
            {Chunk, NextTail} = split_on_last_newline(PrevTail ++ binary_to_list(Bin)),
            spawn(fun () -> Collector ! {dict, scan_chunk(Chunk)} end),

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
    [io:format("~b\t: ~p~n", [V, K]) || {K, V} <- lists:sublist(SortedList, 10)].

scan_chunk(List) -> scan_chunk_1(List, dict:new()).
scan_chunk_1(List, Dict) ->
    case List of
        [] -> Dict;
        "GET /ongoing/When/"++[_,_,_,$x,$/,Y1,Y2,Y3,Y4,$/,M1,M2,$/,D1,D2,$/|Rest] ->
            case match_until_space_newline(Rest, []) of
                {Rest1, []} -> 
                    scan_chunk_1(Rest1, Dict);
                {Rest1, Word} -> 
                    Key = list_to_binary([Y1,Y2,Y3,Y4,$/,M1,M2,$/,D1,D2,$/, Word]),
                    scan_chunk_1(Rest1, dict:update_counter(Key, 1, Dict))
            end;
        [_|Rest] -> scan_chunk_1(Rest, Dict)
    end.
    
match_until_space_newline(List, Word) ->
    case List of
        []     -> {[],   []};
        [10|_] -> {List, []};
        [$.|_] -> {List, []};
        [$ |_] -> {List, lists:reverse(Word)};
        [C|Rest] -> match_until_space_newline(Rest, [C | Word])
    end.
