-module(binbird).


-define(FILE_BUFFER_SIZE, 1024).
%% The best Bin Buffer Size is 4096
-define(BIN_BUFFER_SIZE, 4096).

get_handle(FileName) ->
    erlang:spawn_monitor(fun() -> start(FileName) end).

get_lines({Pid, MRef}) ->
    Pid ! {get_lines, MRef, self()},
    receive
        {{Data, Prep}, MRef} ->
            case travel_bin_eval(Data) of
                [H|T] ->
                    [<<Prep/binary, H/binary>> | T];
                [] ->
                    [Prep]
            end;
        {eof, MRef} ->
            eof;
        {'DOWN', MRef, _Type, Pid, _Info} ->
            erlang:fault(file_closed)
    end.

close({Pid, _}) ->
    Pid ! stop.

start(FileName) ->
    {ok, F} = file:open(FileName, [raw, binary]),
    serve_lines(F, <<>>).

serve_lines(File,Rest) ->
    {Data, Rest1} = get_more_data(File,Rest),
    receive
        {get_lines, Ref, Pid} ->
            Pid ! {Data, Ref},
            serve_lines(File, Rest1);
        stop ->
            file:close(File)
    end.

get_more_data(File, Rest) ->
    case file:read(File, ?FILE_BUFFER_SIZE) of
        eof when size(Rest) =:= 0 ->
            {eof, <<>>};
        eof ->
            {{<<>>, Rest}, <<>>};
        {ok, Bin} ->
            case get_last_line(Bin) of
                {Lines, Rest1} ->
                    {{Lines, Rest}, Rest1};
                Rest1 ->
                    get_more_data(File, <<Rest/binary, Rest1/binary>>)
            end
    end.


%% @doc split Bin on character C
%% -spec(travel_bin_soft_hack/2::Bin::binary(), C:char() -> [binary]). 
get_until(Bin, Str) ->
    StrSize = length(Str),
    get_until_1(Bin, Str, StrSize, 0, []).

get_until_1(Bin, Str, StrSize, Offset, Acc) ->
    %% Compiler will build a decision tree for following binary pattern matchs 
    %% and optimaze it, so, knows how many bits will be traveled and tested.

    case Bin of
        <<_:Offset/binary,Str,_/binary>> ->
            L = Offset,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, Str, 0, [New | Acc]);
        <<_:Offset/binary,_,Str,_/binary>> ->
            L = Offset + StrSize,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, Str, 0, [New | Acc]);
        <<_:Offset/binary,_,_,Str,_/binary>> ->
            L = Offset + 2 * StrSize,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, Str, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,Str,_/binary>> ->
            L = Offset + 3 * StrSize,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, Str, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,_,Str,_/binary>> ->
            L = Offset + 4 * StrSize,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, Str, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,_,_,Str,_/binary>> ->
            L = Offset + 5 * StrSize,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, Str, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,_,_,_,Str,_/binary>> ->
            L = Offset + 6 * StrSize,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, Str, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,_,_,_,_,Str,_/binary>> ->
            L = Offset + 7 * StrSize,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, Str, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,_,_,_,_,_,Str,_/binary>> ->
            L = Offset + 8 * StrSize,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, Str, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,_,_,_,_,_,_,Str,_/binary>> ->
            L = Offset + 9 * StrSize,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, Str, 0, [New | Acc]);
        <<_:Offset/binary,_:80,_/binary>> ->
            split_bin_soft_hack(Bin, Str, Offset + 10  * StrSize, Acc);
        <<_:Offset/binary,_,_/binary>> ->
            split_bin_soft_hack(Bin, Str, Offset + 1 * StrSize, Acc);
        _ ->
            {Bin, lists:reverse(Acc)}
    end.