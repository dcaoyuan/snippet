%% read file sequenctially
-module(tbray9i).
-compile([native]).
-export([start/1]).

-define(READ_SIZE, (1024 * 50000)).
-define(PAT, "/nehW/gniogno/ TEG\" ]"). % lists:reverse("] \"GET /ongoing/When/").
-define(PAT_LEN, 21).    % length("/nehW/gniogno/ TEG\" ]").
-define(SKIP_LEN, 26).   % length("] \"GET /ongoing/When/200x/").
-define(KEY_OFFSET, 37). % length("] \"GET /ongoing/When/200x/2000/10/10/").

start([FileName]) ->
    BmCtx = bm_compile(),
    Ets = ets:new(wfets, [set]),
    Keys = [wait_result(Worker) || Worker <- read_file(FileName, BmCtx)],
    lists:foreach(fun (Key) -> update_counter(Ets, Key) end, lists:append(Keys)),
    print_result(Ets),
    halt().

update_counter(Ets, Key) ->
    case catch ets:update_counter(Ets, Key, 1) of
        {'EXIT', {badarg, _}} -> ets:insert(Ets, {Key, 1});
        _ -> ok
    end.

read_file(FileName, BmCtx) ->
    {ok, File} = file:open(FileName, [raw, binary]),
    read_file_1(File, 0, BmCtx, []).            
read_file_1(File, Offset, BmCtx, Workers) ->
    case file:pread(File, Offset, ?READ_SIZE) of
        eof -> 
            file:close(File),
            Workers;
        {ok, Bin} -> 
            DataL = split_on_first_newline(Bin, size(Bin) - 1, -1),
            Worker = spawn_worker(self(), fun scan_chunk/1, [Bin, DataL, BmCtx], []),
            read_file_1(File, Offset + DataL + 1, BmCtx, [Worker | Workers])
    end.

split_on_first_newline(Bin, S, Direction) ->
    case Bin of
        <<_:S/binary,$\n,_/binary>> -> S + 1;
        <<_:S/binary,_,_/binary>> -> split_on_first_newline(Bin, S + Direction, Direction);
        _ -> S
    end.

scan_chunk([Bin, DataL, BmCtx]) -> scan_chunk_1(Bin, DataL, 0, [], BmCtx).
scan_chunk_1(Bin, DataL, S, Keys, BmCtx) when S < DataL - ?KEY_OFFSET ->
    case bm_match(Bin, S + ?PAT_LEN, ?PAT, BmCtx, 1) of
        {true, _} ->
            case match_until_space_newline(Bin, S + ?KEY_OFFSET, size(Bin)) of
                {true, E} ->
                    Skip = S + ?SKIP_LEN, L = E - Skip,
                    <<_:Skip/binary, Key:L/binary, _/binary>> = Bin,
                    scan_chunk_1(Bin, DataL, E, [Key | Keys], BmCtx);
                {false, E} -> 
                    scan_chunk_1(Bin, DataL, E, Keys, BmCtx)
            end;
        {false, Shift} -> 
            scan_chunk_1(Bin, DataL, S + Shift, Keys, BmCtx)
    end;
scan_chunk_1(_, _, _, Keys, _) -> Keys.

match_until_space_newline(Bin, S, DataL) when S < DataL ->
    case Bin of
        <<_:S/binary,10,_/binary>> -> {false, S};
        <<_:S/binary,$.,_/binary>> -> {false, S};
        <<_:S/binary,_,$ ,_/binary>> -> {true, S + 1};
        _ -> match_until_space_newline(Bin, S + 1, DataL)
    end;
match_until_space_newline(_, S, _) -> {false, S}.

print_result(Tab) ->
    SortedList = lists:reverse(lists:keysort(2, ets:tab2list(Tab))),
    [io:format("~b\t: ~s~n", [V, K]) || {K, V} <- lists:sublist(SortedList, 10)].

spawn_worker(Parent, F, Args, Opts) ->
    erlang:spawn_opt(fun() -> Parent ! {self(), F(Args)} end, [monitor | Opts]).

wait_result({Pid, Ref}) ->
    receive
        {'DOWN', Ref, _, _, normal} -> receive {Pid, Result} -> Result end;
        {'DOWN', Ref, _, _, Reason} -> exit(Reason)
    end.

bm_compile() ->
    Default = dict:from_list([{C, ?PAT_LEN} || C <- lists:seq(1, 255)]),
    Dict = bm_set_shifts(lists:reverse(?PAT), ?PAT_LEN, 1, Default),
    list_to_tuple([Pos || {_, Pos} <- lists:sort(dict:to_list(Dict))]).

bm_set_shifts([C|T], StrLen, Pos, Dict) ->
    bm_set_shifts(T, StrLen, Pos + 1, dict:store(C, StrLen - Pos, Dict));
bm_set_shifts([], _, _, Dict) -> Dict.

bm_match(Bin, S, [C|T], Tab, Count) ->
    case Bin of
        <<_:S/binary, C, _/binary>> -> 
            bm_match(Bin, S - 1, T, Tab, Count + 1);
        <<_:S/binary, C1, _/binary>> ->    
            case element(C1, Tab) of
                ?PAT_LEN -> {false, ?PAT_LEN};
                Shift when Shift < Count -> {false, 1};
                Shift -> {false, Shift - Count + 1}
            end
    end;
bm_match(_, _, [], _, _) -> {true, ?PAT_LEN}. 