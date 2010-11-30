%% Author: Caoyuan Deng, Anders Nygreni, Steve Vinoski
%% travel_bin byte by byte only
%% fatwire-204-87.uniserve.ca (Uniserve On Line) [Label IP Address]
%
%British Columbia, Vancouver, Canada,5 returning visits
%
%Date	Time	WebPage
%November 8th 2007	23:21:38	localhost/ongoing/When/200x/2007/10/30/WF-Results
%blogtrader.net/page/dcaoyuan/entry/tim_bray_s_erlang_exercise2

-module(tbray9f).
-compile([native, inline]).
-export([start/1]).

-define(BUFFER_SIZE, (1024 * 50000)). % best buffer size for file reading
-define(STR_SIZE, 20480). % best buffer size for binary_to_list
-define(PAT_BIN, <<"/nehW/gniogno/ TEG\" ]">>).
-define(PAT, "/nehW/gniogno/ TEG\" ]"). % lists:reverse("] \"GET /ongoing/When/").
-define(PAT_LEN, 21).    % length("/nehW/gniogno/ TEG\" ]").
-define(SKIP_LEN, 26).   % length("] \"GET /ongoing/When/200x/").
-define(KEY_OFFSET, 37). % length("] \"GET /ongoing/When/200x/2000/10/10/").

start([FileName]) ->
    FileSize = filelib:file_size(FileName),
    ProcN = FileSize div ?BUFFER_SIZE,
    ProcN1 = case FileSize rem ?BUFFER_SIZE of 0 -> ProcN; _ -> ProcN + 1 end,
    BmCtx = bm_compile(),
    Results = [wait_result(Worker) || Worker <- read_file(FileName, ?BUFFER_SIZE, ProcN1, BmCtx)],
%    Tid = ets:new(wftab, [set]),
%    Segs = lists:foldl(fun ({Keys, Head, Tail}, Segs) ->
%                               lists:foreach(fun (Key) -> update_tab(Tid, Key) end, Keys),
%                               [Head | [Tail | Segs]]
%                       end, [], Results),
%    Segs1 = [Seg || {_, Seg} <- lists:keysort(1, Segs)],
%    SegKeys = scan_chunk([list_to_binary(Segs1), BmCtx]),
%    lists:foreach(fun (Key) -> update_tab(Tid, Key) end, SegKeys),
%    print_result(Tid),
    halt().

update_tab(Tab, Key) ->
    case catch ets:update_counter(Tab, Key, 1) of
        {'EXIT', {badarg, _}} -> ets:insert(Tab, {Key, 1});
        _ -> ok
    end.

read_file(FileName, Size, ProcN, BmCtx) ->
    [spawn_worker(self(), fun scan_file/1, [FileName, Size, I, BmCtx], [{min_heap_size, 10240}])
     || I <- lists:seq(0, ProcN - 1)].
 
scan_file([FileName, Size, I, BmCtx]) ->
    {ok, File} = file:open(FileName, [read, raw, binary]),
    {ok, Bin} = file:pread(File, Size * I, Size),
    file:close(File),
    traverse_bin(Bin, 0, size(Bin), BmCtx).
%    HeadL = split_on_first_newline(Bin, 0),
%    TailS = split_on_last_newline(Bin, size(Bin)),
%    DataL = TailS - HeadL,
%    <<Head:HeadL/binary, Data:DataL/binary, Tail/binary>> = Bin,
%    {buffered_read(Data), {I * 10, Head}, {I * 10 + 1, Tail}}.

split_on_first_newline(Bin, S) ->
    case Bin of
        <<_:S/binary,$\n,_/binary>> -> S + 1;
        _ -> split_on_first_newline(Bin, S + 1)
    end.

split_on_last_newline(Bin, S) when S > 0 ->
    case Bin of
        <<_:S/binary,$\n,_/binary>> -> S + 1;
        _ -> split_on_last_newline(Bin, S - 1)
    end;
split_on_last_newline(_, S) -> S.

traverse_bin(Bin, S, L, BmCtx) when S < L - 20480 ->
    <<_:S/binary, Buf:20480/binary, _/binary>> = Bin,
    Str = binary_to_list(Buf),
    bm_match(Str, S + ?PAT_LEN - 1, ?PAT, BmCtx, 0),
    %scan_chunk([Buf, BmCtx]),
    traverse_bin(Bin, S + 20480, L, BmCtx);
traverse_bin(_, _, _, _) -> done.

scan_chunk([Bin, BmCtx]) -> scan_chunk_1(Bin, size(Bin), 0, [], BmCtx).
scan_chunk_1(Bin, DataL, S, Keys, BmCtx) when S < DataL - ?KEY_OFFSET ->
    case bm_match(Bin, S + ?PAT_LEN - 1, ?PAT, BmCtx, 0) of
        {true, _} ->
            case match_until_space_newline(Bin, S + ?KEY_OFFSET, size(Bin)) of
                {true, E} ->
                    Skip = S + ?SKIP_LEN, L = E - Skip,
                    <<_:Skip/binary, Key:L/binary, _/binary>> = Bin,
                    scan_chunk_1(Bin, DataL, E + 1, [Key | Keys], BmCtx);
                {false, E} -> 
                    scan_chunk_1(Bin, DataL, E + 1, Keys, BmCtx)
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
    
spawn_worker(Parent, F, Args, Opts) ->
    erlang:spawn_opt(fun() -> Parent ! {self(), F(Args)} end, [monitor | Opts]).

wait_result({Pid, Ref}) ->
    receive
        {'DOWN', Ref, _, _, normal} -> receive {Pid, Result} -> Result end;
        {'DOWN', Ref, _, _, Reason} -> exit(Reason)
    end.

print_result(Tab) ->
    SortedList = lists:reverse(lists:keysort(2, ets:tab2list(Tab))),
    [io:format("~b\t: ~s~n", [V, K]) || {K, V} <- lists:sublist(SortedList, 10)].
    
bm_compile() ->
    Default = dict:from_list([{C, ?PAT_LEN} || C <- lists:seq(1, 255)]),
    Dict = bm_set_shifts(lists:reverse(?PAT), ?PAT_LEN, 1, Default),
    list_to_tuple([Pos || {_, Pos} <- lists:sort(dict:to_list(Dict))]).

bm_set_shifts([], _, _, Dict) -> Dict;
bm_set_shifts([C|T], StrLen, Pos, Dict) ->
    bm_set_shifts(T, StrLen, Pos + 1, dict:store(C, StrLen - Pos, Dict)).

bm_match([C1|Rest], S, [C|T], Tab, Count) ->
    case C1 of
        C ->
            bm_match(Rest, S - 1, T, Tab, Count + 1);
        _ ->
            case element(C1, Tab) of
                ?PAT_LEN -> {false, ?PAT_LEN};
                Shift when Shift =< Count -> {false, 1};
                Shift -> {false, Shift - Count}
            end
    end;
bm_match([], _, _, _, _) -> {true, ?PAT_LEN}. 
