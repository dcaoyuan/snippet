%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Author  : Caoyuan Deng <dcaoyuan@lightpole.net>
%%% Description :
%%%
%%% Created : 25 Sep 2007 by Per Gustafsson <pergu@it.uu.se>
%%% Modified: 01 Oct 2007 by Caoyuan Deng <dcaoyuan@lightpole.net> 
%%%-------------------------------------------------------------------
-module(line_server).

-export([get_handle/1,get_lines/1,close/1]).

-compile([native]).

-define(FILE_BUFFER_SIZE, 70000).
%% The best Bin Buffer Size is 4096
-define(BIN_BUFFER_SIZE, 4096).

get_handle(FileName) ->
    erlang:spawn_monitor(fun() -> start(FileName) end).

get_lines({Pid, MRef}) ->
    Pid ! {get_lines, MRef, self()},
    receive
        {{Data, Prep}, MRef} ->
            case divide_lines(Data) of
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

get_last_line(Bin) ->
    get_last_newline(Bin, size(Bin) - 1).

get_last_newline(Bin, S) ->
    case Bin of
        <<Lines:S/binary,10,Rest/binary>> ->
            {Lines, Rest};
        _ ->
            if S =< 0 -> Bin;
                true -> get_last_newline(Bin, S - 1)
            end
    end.


travel_bin_eval(Bin) -> travel_bin_eval(Bin, 0, []).
travel_bin_eval(Bin, Offset, Acc) ->
    case Bin of
        <<_:Offset/binary,$\n,_/binary>> ->
            travel_bin_eval(Bin, Offset + 1, [Offset | Acc]);
        <<_:Offset/binary,_,_/binary>> ->
            travel_bin_eval(Bin, Offset + 1, Acc);
        _ -> 
            split_bin(Bin, lists:reverse(Acc))
    end.

split_bin(Bin, PosList) -> split_bin(Bin, PosList, 1, -1, "<<").    
split_bin(Bin, PosList, Count, PrePos, Expr) ->
    case PosList of
        [Pos|Rest] ->
            Len = Pos - PrePos - 1,
            Expr1 = Expr ++ "L" ++ integer_to_list(Count) ++ ":" ++ integer_to_list(Len) ++ "/binary,_,",
            split_bin(Bin, Rest, Count + 1, Pos, Expr1);
        [] ->
            Expr1 = Expr ++ "R" ++ "/binary>> = Bin",
            %io:format("Expr: ~p~n", [Expr1]),
            BindingDict = erl_eval:add_binding('Bin', Bin, erl_eval:new_bindings()),
            [_H|RestLines] = term_from_expr(Expr1, BindingDict),
            [Line || {_, Line} <- RestLines]
    end.

term_from_expr(Expr, Bindings) ->
    {ok, ExprsTokens, _EndLine} = erl_scan:string(Expr ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(ExprsTokens),
    {value, _Value, NewBindings} = erl_eval:exprs(Exprs, Bindings),
    NewBindings.

divide_lines(Bin) ->
    divide_lines(Bin, 0, []).

divide_lines(Bin, S, Acc) ->
    case Bin of
        <<_:S/binary,10,_/binary>> ->
            L = S,
            <<New:L/binary, _, Rest/binary>> = Bin,
            divide_lines(Rest, 0, [New | Acc]);
        <<_:S/binary,_,10,_/binary>> ->
            L = S + 1,
            <<New:L/binary, _, Rest/binary>> = Bin,
            divide_lines(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,10,_/binary>> ->
            L = S + 2,
            <<New:L/binary, _, Rest/binary>> = Bin,
            divide_lines(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,10,_/binary>> ->
            L = S + 3,
            <<New:L/binary, _, Rest/binary>> = Bin,
            divide_lines(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,10,_/binary>> ->
            L = S + 4,
            <<New:L/binary, _, Rest/binary>> = Bin,
            divide_lines(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,10,_/binary>> ->
            L = S + 5,
            <<New:L/binary, _, Rest/binary>> = Bin,
            divide_lines(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,_,10,_/binary>> ->
            L = S + 6,
            <<New:L/binary, _, Rest/binary>> = Bin,
            divide_lines(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,_,_,10,_/binary>> ->
            L = S + 7,
            <<New:L/binary, _, Rest/binary>> = Bin,
            divide_lines(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,_,_,_,10,_/binary>> ->
            L = S + 8,
            <<New:L/binary, _, Rest/binary>> = Bin,
            divide_lines(Rest, 0, [New | Acc]);
        <<_:S/binary,_,_,_,_,_,_,_,_,_,10,_/binary>> ->
            L = S + 9,
            <<New:L/binary, _, Rest/binary>> = Bin,
            divide_lines(Rest, 0, [New | Acc]);
        <<_:S/binary,_:80,_/binary>> ->
            divide_lines(Bin, S + 10, Acc);
        <<_:S/binary,_,_/binary>> ->
            divide_lines(Bin, S + 1, Acc);
        _ ->
            lists:reverse([Bin | Acc])
    end.


