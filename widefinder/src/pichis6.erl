-module(pichis6).

-compile([debug_info, native, {hipe, [o3]}]).

-export([start/1, start/2, start/3]).
-export([file_open/3, file_read/1, file_close/1, file_test_read/3]).
-export([fold_reduce_file/6]).

start([FileName, N]) -> start(FileName, 1024*32, list_to_integer(N));
start(FileName) -> start(FileName, 1024*32, 1).
start(FileName, N) -> start(FileName, 1024*32, N).
start(FileName, ChunkSize, N) ->
    Start = now(),
    Result = fold_reduce_file(
        FileName,
        _Acc0 = dict:new(),
        _Folderer = fun (Acc, Chunk) -> scan(Chunk, Acc) end,
        _Reducer = fun (Acc, Result) -> dict:merge(
        fun(_,V1,V2) -> V1+V2 end,
        Acc,
        Result) end,
        ChunkSize,
        N
                             ),
    Delta = timer:now_diff(now(), Start) / 1000,
    print_result(Result),
    if
        Delta > 1000 -> io:format("Time: ~.3f s~n", [Delta/1000]);
        true -> io:format("Time: ~.3f ms~n", [Delta])
    end,
    ok.

print_result(Dict) ->
    [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10 | _] = lists:reverse(lists:keysort(2, dict:to_list(Dict))),
    lists:foreach(fun ({Word, Count}) ->
                          io:format("~p get requests for ~s~n", [Count, Word])
                  end, [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10]).

scan("GET /ongoing/When/" ++ [_,_,_,$x,$/,Y1,Y2,Y3,Y4,$/,M1,M2,$/,D1,D2,$/|Rest], Dict) ->
    case scan_key(Rest) of
        {[_|_] = Key, NewRest} ->
            scan(NewRest, dict:update_counter([Y1,Y2,Y3,Y4,$/,M1,M2,$/,D1,D2,$/|Key], 1, Dict));
        {[], NewRest} -> scan(NewRest, Dict)
    end;
scan([_|Rest], Dict) -> scan(Rest, Dict);
scan([], Dict) -> Dict.

scan_key(L) -> scan_key(L, []).

scan_key([$ |Rest], Key) -> {lists:reverse(Key), Rest};
scan_key([$\n|Rest], _) -> {[], Rest};
scan_key([$.|Rest], _) -> {[], Rest};
scan_key([C|Rest], Key) -> scan_key(Rest, [C|Key]);
scan_key([], _) -> {[],[]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Proof of concept of fold&reduce
% on file by new line terminated chunks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(context, {acc,
                  chunkNum,
                  processedNum = 0,
                  reducer}).

fold_reduce_file(FileName, Acc0, Folderer, Reducer, ChunkSize, N) ->
    {ok, CR} = file_open(FileName, ChunkSize, nlt_chunk),
    M = self(),
    do_n(fun() ->
                 spawn_link(fun()-> folderer(CR, Acc0, Folderer, M) end)
         end, 0, N),
    Result = collect_loop(#context{acc=Acc0,
                                   chunkNum=N,
                                   reducer=Reducer}),
    file_close(CR),
    Result.

do_n(What, Start, Stop) when Start < Stop -> What(), do_n(What, Start+1, Stop);
do_n(_, _, _) -> ok.

folderer(CR, Acc0, Folderer, Collector) ->
    case file_read(CR) of
        {ok, {A, B}} ->
            folderer(CR, Folderer(
                Acc0,
                binary_to_list(A) ++ binary_to_list(B)
                                 ), Folderer, Collector);
        eof ->
            Collector ! {result, Acc0}
    end.

collect_loop(#context{acc=Acc0,
                      chunkNum=ChunkNum,
                      processedNum=ProcessedNum,
                      reducer=Reducer}=Context) ->
    case ProcessedNum of
        ChunkNum ->
            Acc0;
        _ ->
            receive
                {result, Result} ->
                    collect_loop(Context#context{acc = Reducer(Acc0, Result),
                                                 processedNum = ProcessedNum+1})
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chunk reader process with read ahead
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_open(FileName, ChunkSize, chunk) ->        % raw chunks
    M = self(),
    {ok, {chunk_reader, 
          spawn_link(fun() ->
                             {ok, File} = file:open(FileName, [read, raw, binary]),
                             process_flag(trap_exit, true),
                             process_flag(priority, high),
                             file_loop(M, File, file:read(File, ChunkSize), ChunkSize)
                     end)}};
file_open(FileName, ChunkSize, nlt_chunk) ->    % new line terminated chunks
    M = self(),
    {ok, {nlt_chunk_reader, 
          spawn_link(fun() ->
                             {ok, CR} = file_open(FileName, ChunkSize, chunk),
                             process_flag(trap_exit, true),
                             process_flag(priority, high),
                             {ok, First_Read} = file_read(CR),
                             cr_loop(M,
                                     CR,
                                     cr_read_n_split(CR, First_Read, file_read(CR)))
                     end)}}.

file_read({Type, Pid}) when Type == chunk_reader; Type == nlt_chunk_reader ->
    case is_process_alive(Pid) of
        true ->
            Pid ! {read, self()},
            receive
                {ok, B} -> {ok, B};
                eof -> eof
            after 60000 -> timeout   % Possible race condition with is_process_alive
            end;
        false -> error
    end.

file_close({Type, Pid}) when Type == chunk_reader; Type == nlt_chunk_reader ->
    case is_process_alive(Pid) of
        true -> Pid ! close, ok;
        false -> error
    end.

file_loop(Master, File, Chunk, ChunkSize) ->
    receive
        {read, From} ->
            From ! Chunk,
            case Chunk of
                {ok, _} ->
                    file_loop(Master, File, file:read(File, ChunkSize), ChunkSize);
                eof ->
                    file:close(File),
                    file_eof_loop(Master)
            end;
        close -> file:close(File);
        {'EXIT', Master, _} -> file:close(File);
        _ -> file_loop(Master, File, Chunk, ChunkSize)  % ignore unknow
    end.

file_eof_loop(Master) ->  % wait for eof request
    receive
        {read, From} ->
            From ! eof,
            file_eof_loop(Master);
        close -> ok;
        {'EXIT', Master, _} -> ok;
        _ -> file_eof_loop(Master)
    end.

cr_loop(Master, CR, {Prev, Line, Next}) ->
    receive
        {read, From} ->
            From ! {ok, {Prev, Line}},
            case Next of
                _ when is_binary(Next) ->
                    cr_loop(Master, CR, cr_read_n_split(CR, Next, file_read(CR)));
                eof ->
                    file_close(CR),
                    file_eof_loop(Master)
            end;
        close -> file_close(CR);
        {'EXIT', Master, _} -> file_close(CR);
        _ -> cr_loop(Master, CR, {Prev, Line, Next})    % ignore unknow
    end.

cr_read_n_split(CR, Prev, {ok, B}) ->
    case split_on_nl(B) of
        {Line, Rest} when is_binary(Rest) ->    % nonempty remaining part
            { Prev, Line, Rest };
        {Line, none} -> % new line not found, read again, should be very rare
            cr_read_n_split(CR, <<Prev/binary, Line/binary>>, file_read(CR))
    end;
cr_read_n_split(_CR, Prev, eof) ->
    {<<>>, Prev, eof}.  % easier joining at this order

split_on_nl(B) -> split_on_nl(B, 0, size(B)).

split_on_nl(B, N, S) when N < S ->
    case B of
        <<Line:N/binary, $\n, Tail/binary>> -> {Line, Tail};
        _ -> split_on_nl(B, N+1, S)
    end;
split_on_nl(B, _, _) -> {B, none}.

% speed testing functions
file_test_read(FileName, ChunkSize, Type) ->
    {ok, File} = file_open(FileName, ChunkSize, Type),
    eof = file_test_read_loop(File, file_read(File)),
    file_close(File).

file_test_read_loop(File, {ok, _}) ->
    file_test_read_loop(File, file_read(File));
file_test_read_loop(_, eof) ->
    eof.
