-module(list_bin).

-export([test_append/0]).

test_append() -> 
    test_chr_append(100),
    test_chr_append(1000),
    test_chr_append(10000),
    test_chr_append(100000),
    %test_char_append(1000000),
    %test_char_append(10000000),
    test_str_append(100),
    test_str_append(1000),
    test_str_append(10000),
    test_str_append(100000),
    %test_field_append(200000),
    %test_field_append(300000).
    concat_bin().

test_chr_append(N) ->
    erlang:statistics(wall_clock),

    test_chr_append_by_lst(N, []),
    {_, T1} = erlang:statistics(wall_clock),
    
    test_chr_append_by_bin(N, <<>>),
    {_, T2} = erlang:statistics(wall_clock),
    
    io:format("~p loops, test_chr_append_by_lst using time: ~pms~n", [N, T1]),
    io:format("~p loops, test_chr_append_by_bin using time: ~pms~n~n", [N, T2]),
    ok.

test_chr_append_by_lst(0, List) -> lists:reverse(List);
test_chr_append_by_lst(N, List) -> 
    test_chr_append_by_lst(N-1, [$!|List]).

test_chr_append_by_bin(0, Bin) -> binary_to_list(Bin);
test_chr_append_by_bin(N, Bin) -> 
    test_chr_append_by_bin(N-1, <<Bin/binary, $!>>).

%% // -------- str concat
test_str_append(N) ->
    Str = lists:duplicate(100, $a),
    BinStr = list_to_binary(Str),

    erlang:statistics(wall_clock),

    test_str_append_by_lst(Str, N, []),
    {_, T1} = erlang:statistics(wall_clock),
    
    test_str_append_by_bin(BinStr, N, []),
    {_, T2} = erlang:statistics(wall_clock),
    
    io:format("~p loops, test_str_append_by_lst using time: ~pms~n", [N, T1]),
    io:format("~p loops, test_str_append_by_bin using time: ~pms~n~n", [N, T2]),
    ok.
    
test_str_append_by_lst(_, 0, List) -> lists:flatten(List);
test_str_append_by_lst(Str, N, List) -> 
    test_str_append_by_lst(Str, N-1, [Str|List]).

test_str_append_by_bin(_, 0, List) -> list_to_binary(List);
test_str_append_by_bin(Str, N, List) -> 
    test_str_append_by_bin(Str, N-1, [Str|List]).

concat_bin() ->
    list_to_binary([<<"This">>, <<" is ">>, <<"a ">>, <<"Test">>]).
