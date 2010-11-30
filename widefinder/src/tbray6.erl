-module(tbray6).
-compile([native]).
-export([start/1]).

-define(BUFFER_SIZE, (1024 * 10000)).

start(FileName) ->
    Dicts = [wait_result(Worker) || Worker <- read_file(FileName)],
    print_result(merge_dicts(Dicts)).
              
read_file(FileName) ->
    {ok, File} = file:open(FileName, [raw, binary]),
    read_file_1(File, 0, []).            
read_file_1(File, Offset, Workers) ->
    case file:pread(File, Offset, ?BUFFER_SIZE) of
        eof -> 
            file:close(File),
            Workers;
        {ok, Bin} -> 
            DataL = split_on_last_newline(Bin),
            Worker = spawn_worker(self(), fun scan_chunk/1, {Bin, DataL}),
            read_file_1(File, Offset + DataL + 1, [Worker | Workers])
    end.

split_on_last_newline(Bin) -> split_on_last_newline_1(Bin, size(Bin)).   
split_on_last_newline_1(Bin, S) when S > 0 ->
    case Bin of
        <<_:S/binary,$\n,_/binary>> -> S;
        _ -> split_on_last_newline_1(Bin, S - 1)
    end;
split_on_last_newline_1(_, S) -> S.


%% This version will split Bin to Rest, and apply to Rest recursively
scan_chunk({Bin, DataL}) -> scan_chunk_1(Bin, DataL, 0, dict:new()).
scan_chunk_1(Bin, DataL, S, Dict) when S < DataL - 34 ->
    case Bin of
        <<_:S/binary,"GET /ongoing/When/",_,_,_,$x,$/,_,_,_,_,$/,_,_,$/,_,_,$/,_/binary>> ->
            case match_until_space_newline(Bin, S + 34) of
                {true, E} ->
                    Skip = S + 23, L = E - Skip,
                    <<_:Skip/binary,Key:L/binary,Rest/binary>> = Bin,
                    scan_chunk_1(Rest, DataL - E, 0, dict:update_counter(Key, 1, Dict));
                {false, E} ->
                    <<_:E/binary,Rest/binary>> = Bin,
                    scan_chunk_1(Rest, DataL - E, 0, Dict)
            end;
        _ -> scan_chunk_1(Bin, DataL, S + 1, Dict)
    end;
scan_chunk_1(_, _, _, Dict) -> Dict.

match_until_space_newline(Bin, S) when S < size(Bin) ->
    case Bin of
        <<_:S/binary,10,_/binary>> -> {false, S};
        <<_:S/binary,$.,_/binary>> -> {false, S};
        <<_:S/binary,_,$ ,_/binary>> -> {true, S + 1};
        _ -> match_until_space_newline(Bin, S + 1)
    end;
match_until_space_newline(_, S) -> {false, S}.
    
spawn_worker(Parent, F, A) ->
    erlang:spawn_monitor(fun() -> Parent ! {self(), F(A)} end).

wait_result({Pid, Ref}) ->
    receive
        {'DOWN', Ref, _, _, normal} -> receive {Pid, Result} -> Result end;
        {'DOWN', Ref, _, _, Reason} -> exit(Reason)
    end.
    
merge_dicts([D1,D2|Rest]) ->
    merge_dicts([dict:merge(fun(_, V1, V2) -> V1 + V2 end, D1, D2) | Rest]);
merge_dicts([D]) -> D.

print_result(Dict) ->
    SortedList = lists:reverse(lists:keysort(2, dict:to_list(Dict))),
    [io:format("~b\t: ~p~n", [V, K]) || {K, V} <- lists:sublist(SortedList, 10)].
    