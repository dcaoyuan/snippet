-module(traverse).

-export([traverse_bin_bad/1,
         traverse_bin/1,
         traverse_bin_eval/1,
         traverse_bin_hard_hack/1,
         traverse_bin_soft_hack/1,
         traverse_list/1,
         traverse_list_bin/1,
         traverse_list_hack/1,
         traverse_list_buffered/1,
         traverse_tuple/1,
         start/1]).

-define(BIN_BUFFER_SIZE, 4096).


%46> traverse:start("o10.ap").  
%traverse_bin_bad, time in ms:              432
%traverse_bin, time in ms:                  471
%traverse_bin_hard_hack, time in ms:         88
%traverse_bin_soft_hack, time in ms:        131
%traverse_list, time in ms:                 161
%traverse_list_buffered, time in ms:        140
%traverse_tuple, time in ms:                176
%ok
%47> traverse:start("o10k.ap").
%traverse_bin_bad, time in ms:              305501
%traverse_bin, time in ms:                  317298
%traverse_bin_hard_hack, time in ms:         68562
%traverse_bin_soft_hack, time in ms:        137328
%traverse_list, time in ms:                 314561
%traverse_list_buffered, time in ms:        213148
%traverse_tuple, time in ms:                200581
%ok
%48> traverse:start("o100k.ap").
%traverse_bin_bad, time in ms:              2994559
%traverse_bin, time in ms:                  1930907
%traverse_bin_hard_hack, time in ms:         535072
%traverse_bin_soft_hack, time in ms:         955383
%traverse_list, time in ms:                 3989314
%traverse_list_buffered, time in ms:        2740261
%traverse_tuple, time in ms:                2019784
%ok


start(FileName) ->
    {ok, Bin} = file:read_file(FileName),

    lists:foreach(
      fun (Fun) ->
              {Time, _} = timer:tc(?MODULE, Fun, [Bin]),
              io:format("~10b ms:\t ~p~n", [Time, Fun])
      end, [traverse_bin_bad, 
            traverse_bin, 
            traverse_bin_hard_hack,
            traverse_bin_soft_hack,
            traverse_list,
            traverse_list_hack,
            traverse_list_buffered,
            traverse_tuple]).

traverse_bin_bad(Bin) -> traverse_bin_bad(Bin, 0). 
traverse_bin_bad(Bin, LineCount) ->
    case Bin of
        <<$\n,Rest/binary>> ->
            traverse_bin_bad(Rest, LineCount); 
        <<_,Rest/binary>> ->
            traverse_bin_bad(Rest, LineCount);
        <<>> -> LineCount
    end. 

traverse_bin(Bin) -> traverse_bin(Bin, 0, []). 
traverse_bin(Bin, Offset, Acc) ->
    case Bin of
        <<Line:Offset/binary,$\n,Rest/binary>> ->
            traverse_bin(Rest, 0, [Line | Acc]);
        <<_:Offset/binary,_,_/binary>> ->
            traverse_bin(Bin, Offset + 1, Acc);
        _ -> {Bin, lists:reverse(Acc)}
    end.

traverse_bin_hard_hack(Bin) -> traverse_bin_hard_hack(Bin, 0, []).
traverse_bin_hard_hack(Bin, Offset, Acc) -> 
    %% Compiler will build a decision tree for following binary pattern matchs 
    %% and optimaze it, so, knows how many bits will be traverseed and tested.
    case Bin of
        <<_:Offset/binary,10,_/binary>> ->
            L = Offset,
            <<Line:L/binary, _, Rest/binary>> = Bin,
            traverse_bin_hard_hack(Rest, 0, [Line | Acc]);
        <<_:Offset/binary,_,10,_/binary>> ->
            L = Offset + 1,
            <<Line:L/binary, _, Rest/binary>> = Bin,
            traverse_bin_hard_hack(Rest, 0, [Line | Acc]);
        <<_:Offset/binary,_,_,10,_/binary>> ->
            L = Offset + 2,
            <<Line:L/binary, _, Rest/binary>> = Bin,
            traverse_bin_hard_hack(Rest, 0, [Line | Acc]);
        <<_:Offset/binary,_,_,_,10,_/binary>> ->
            L = Offset + 3,
            <<Line:L/binary, _, Rest/binary>> = Bin,
            traverse_bin_hard_hack(Rest, 0, [Line | Acc]);
        <<_:Offset/binary,_,_,_,_,10,_/binary>> ->
            L = Offset + 4,
            <<Line:L/binary, _, Rest/binary>> = Bin,
            traverse_bin_hard_hack(Rest, 0, [Line | Acc]);
        <<_:Offset/binary,_,_,_,_,_,10,_/binary>> ->
            L = Offset + 5,
            <<Line:L/binary, _, Rest/binary>> = Bin,
            traverse_bin_hard_hack(Rest, 0, [Line | Acc]);
        <<_:Offset/binary,_,_,_,_,_,_,10,_/binary>> ->
            L = Offset + 6,
            <<Line:L/binary, _, Rest/binary>> = Bin,
            traverse_bin_hard_hack(Rest, 0, [Line | Acc]);
        <<_:Offset/binary,_,_,_,_,_,_,_,10,_/binary>> ->
            L = Offset + 7,
            <<Line:L/binary, _, Rest/binary>> = Bin,
            traverse_bin_hard_hack(Rest, 0, [Line | Acc]);
        <<_:Offset/binary,_,_,_,_,_,_,_,_,10,_/binary>> ->
            L = Offset + 8,
            <<Line:L/binary, _, Rest/binary>> = Bin,
            traverse_bin_hard_hack(Rest, 0, [Line | Acc]);
        <<_:Offset/binary,_,_,_,_,_,_,_,_,_,10,_/binary>> ->
            L = Offset + 9,
            <<Line:L/binary, _, Rest/binary>> = Bin,
            traverse_bin_hard_hack(Rest, 0, [Line | Acc]);
        <<_:Offset/binary,_:80,_/binary>> ->
            traverse_bin_hard_hack(Bin, Offset + 10, Acc);
        <<_:Offset/binary,_,_/binary>> ->
            traverse_bin_hard_hack(Bin, Offset + 1, Acc);
        _ ->
            {Bin, lists:reverse(Acc)}
    end.


traverse_bin_soft_hack(Bin) -> split_bin_soft_hack(Bin, $\n).

%% @doc split Bin on character C
  %% -spec(traverse_bin_soft_hack/2::Bin::binary(), C:char() -> [binary]). 
split_bin_soft_hack(Bin, C) ->
    split_bin_soft_hack(Bin, C, 0, []).

split_bin_soft_hack(Bin, C, Offset, Acc) ->
    %% Compiler will build a decision tree for following binary pattern matchs 
    %% and optimaze it, so, knows how many bits will be traverseed and tested.
    case Bin of
        <<_:Offset/binary,C,_/binary>> ->
            L = Offset,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, C, 0, [New | Acc]);
        <<_:Offset/binary,_,C,_/binary>> ->
            L = Offset + 1,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, C, 0, [New | Acc]);
        <<_:Offset/binary,_,_,C,_/binary>> ->
            L = Offset + 2,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, C, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,C,_/binary>> ->
            L = Offset + 3,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, C, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,_,C,_/binary>> ->
            L = Offset + 4,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, C, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,_,_,C,_/binary>> ->
            L = Offset + 5,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, C, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,_,_,_,C,_/binary>> ->
            L = Offset + 6,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, C, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,_,_,_,_,C,_/binary>> ->
            L = Offset + 7,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, C, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,_,_,_,_,_,C,_/binary>> ->
            L = Offset + 8,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, C, 0, [New | Acc]);
        <<_:Offset/binary,_,_,_,_,_,_,_,_,_,C,_/binary>> ->
            L = Offset + 9,
            <<New:L/binary, _, Rest/binary>> = Bin,
            split_bin_soft_hack(Rest, C, 0, [New | Acc]);
        <<_:Offset/binary,_:80,_/binary>> ->
            split_bin_soft_hack(Bin, C, Offset + 10, Acc);
        <<_:Offset/binary,_,_/binary>> ->
            split_bin_soft_hack(Bin, C, Offset + 1, Acc);
        _ -> {Bin, lists:reverse(Acc)}
    end.


traverse_list(Bin) when is_binary(Bin) -> traverse_list(binary_to_list(Bin));
traverse_list(List) -> traverse_list(List, [], []). 
traverse_list(Bin, LineBuf, Acc) when is_binary(Bin) -> traverse_list(binary_to_list(Bin), LineBuf, Acc);
traverse_list(List, LineBuf, Acc) -> 
    case List of
        [$\n|Rest] ->
            Line = lists:reverse(LineBuf),
            %% if cons Acc with list Line, when pass to next fun, will copy it,
            %% if cons Acc with binary Line, no need to copy 
            %traverse_list(Rest, [], [Line | Acc]);
            traverse_list(Rest, [], [list_to_binary(Line) | Acc]);
        [C|Rest] ->
            traverse_list(Rest, [C | LineBuf], Acc); 
        [] -> {LineBuf, lists:reverse(Acc)}
    end.

traverse_list_hack(Bin) when is_binary(Bin) -> traverse_list_hack(binary_to_list(Bin));
traverse_list_hack(List) -> traverse_list_hack(List, [], []). 
traverse_list_hack(Bin, LineBuf, Acc) when is_binary(Bin) -> 
    traverse_list_hack(binary_to_list(Bin), LineBuf, Acc);
traverse_list_hack(List, LineBuf, Acc) -> 
    case List of
        [$\n|Rest] ->
            Line = lists:reverse(LineBuf),
            traverse_list_hack(Rest, [], [list_to_binary(Line) | Acc]);
        [C1,$\n|Rest] ->
            Line = lists:reverse([C1 | LineBuf]),
            traverse_list_hack(Rest, [], [list_to_binary(Line) | Acc]);
        [C1,C2,$\n|Rest] ->
            Line = lists:reverse([C2 | [C1 | LineBuf]]),
            traverse_list_hack(Rest, [], [list_to_binary(Line) | Acc]);
        [C1,C2,C3,$\n|Rest] ->
            Line = lists:reverse([C3 | [C2 | [C1 | LineBuf]]]),
            traverse_list_hack(Rest, [], [list_to_binary(Line) | Acc]);
        [C1,C2,C3,C4|Rest] ->
            traverse_list_hack(Rest, [C4 | [C3 | [C2 | [C1 | LineBuf]]]], Acc);
        [C1|Rest] ->
            traverse_list_hack(Rest, [C1 | LineBuf], Acc); 
        [] -> {LineBuf, lists:reverse(Acc)}
    end.

traverse_list_bin(Bin) -> traverse_list_bin(Bin, <<>>, []). 
traverse_list_bin(Bin, LineBuf, Acc) -> 
    List = binary_to_list(Bin),
    case List of
        [$\n|Rest] ->
            Line = lists:reverse(binary_to_list(LineBuf)),
            %io:format("~n~p~n", [Line]),
            %% if cons Acc with list Line, when pass to next fun, will copy it,
            %% if cons Acc with binary Line, no need to copy 
            %traverse_list(Rest, [], [Line | Acc]);
            traverse_list_bin(list_to_binary(Rest), <<>>, [list_to_binary(Line) | Acc]);
        [C|Rest] ->
            traverse_list_bin(list_to_binary(Rest), list_to_binary([C | LineBuf]), Acc); 
        [] -> {LineBuf, lists:reverse(Acc)}
    end.


traverse_list_buffered(Bin) ->
    buffered_read(fun (Buf, {PrevTail, Acc}) -> 
                          traverse_list(Buf, PrevTail, Acc)
                  end, {[], []}, Bin).
    
buffered_read(Fun, Acc, Bin) ->
    case Bin of
        <<Buf:?BIN_BUFFER_SIZE/binary, Rest/binary>> ->
            buffered_read(Fun, Fun(Buf, Acc), Rest);
        _ -> Fun(Bin, Acc)
    end.

traverse_tuple(Bin) when is_binary(Bin) -> traverse_tuple(binary_to_list(Bin));
traverse_tuple(List) when is_list(List) -> traverse_tuple(list_to_tuple(List));
traverse_tuple(Tuple) -> traverse_tuple(Tuple, 0, size(Tuple), 1). 
traverse_tuple(_, LineCount, Size, I) when I > Size -> LineCount;
traverse_tuple(Tuple, LineCount, Size, I) ->
    LineCount1 = if  element(I, Tuple) == $\n -> LineCount + 1;
                     true -> LineCount end,
    traverse_tuple(Tuple, LineCount1, Size, I + 1). 



%% Keep for reference.
traverse_bin_eval(Bin) -> traverse_bin_eval(Bin, 0, []).
traverse_bin_eval(Bin, Offset, Acc) ->
    case Bin of
        <<_:Offset/binary,$\n,_/binary>> ->
            traverse_bin_eval(Bin, Offset + 1, [Offset | Acc]);
        <<_:Offset/binary,_,_/binary>> ->
            traverse_bin_eval(Bin, Offset + 1, Acc);
        _ -> 
            split_bin(Bin, lists:reverse(Acc))
    end.

split_bin(Bin, PosList) -> split_bin(Bin, PosList, 1, -1, "<<").    
split_bin(Bin, PosList, Count, PrevPos, Expr) ->
    case PosList of
        [Pos|Rest] ->
            Len = Pos - PrevPos - 1,
            Expr1 = [Expr, "L", integer_to_list(Count), ":", integer_to_list(Len), "/binary,_,"],
            split_bin(Bin, Rest, Count + 1, Pos, Expr1);
        [] ->
            Expr1 = [Expr, "R", "/binary>> = Bin"],
            %io:format("Expr: ~p~n", [Expr1]),
            BindingDict = erl_eval:add_binding('Bin', Bin, erl_eval:new_bindings()),
            [_|RestLines] = term_from_expr(lists:flatten(Expr1), BindingDict),
            [Line || {_, Line} <- RestLines]
    end.

term_from_expr(Expr, Bindings) ->
    {ok, ExprsTokens, _EndLine} = erl_scan:string(Expr ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(ExprsTokens),
    {value, _Value, NewBindings} = erl_eval:exprs(Exprs, Bindings),
    NewBindings.
    

