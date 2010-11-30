-module(btest).

-export([get_nth_word/2,
         get_nth_word_bad/2]).
         
-export([split_words/3]).

get_nth_word(Bin, N) ->
    Offsets = calc_word_offsets(Bin, 0, [0]),
    S = element(N, Offsets),
    E = element(N + 1, Offsets),
    L = E - S,
    <<_:S/binary,Word:L/binary,_/binary>> = Bin,
    io:format("nth Word: ~p", [Word]).

calc_word_offsets(Bin, Offset, Acc) when Offset < size(Bin) ->
    case Bin of
        <<_:Offset/binary,$ ,_/binary>> -> 
            calc_word_offsets(Bin, Offset + 1, [Offset + 1 | Acc]);
        _ ->
            calc_word_offsets(Bin, Offset + 1, Acc)
    end;
calc_word_offsets(_, _, Acc) -> list_to_tuple(lists:reverse(Acc)).
    

get_nth_word_bad(Bin, N) ->
    Words = split_words(Bin, 0, []),
    Word = element(N, Words),
    io:format("nth Word: ~p", [Word]).

split_words(Bin, Offset, Acc) ->
    case Bin of
        <<Word:Offset/binary,$ ,Rest/binary>> ->
            split_words(Rest, 0, [Word | Acc]);
        <<_:Offset/binary,_,_/binary>> ->
            split_words(Bin, Offset + 1, Acc);
        _ -> list_to_tuple(lists:reverse([Bin | Acc]))
    end.

    