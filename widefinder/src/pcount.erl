%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : Counts get requests in Tim Bray's logfiles
%%%
%%% Created : 23 Sep 2007 by Per Gustafsson <pergu@c83-253-58-61.bredband.comhem.se>
%%%-------------------------------------------------------------------
-module(pcount).

-export([run/1,main/2]).

-compile([native]).
 
%%-type(handle()::any()).
%%-type(dict()::any()).

%%-spec(run/1::([string()]) -> unit()).

%% run should be evoked from the commandline with:
%% erl -smp -noshell -run pcount run filename n
%%
%% filename is the name of the log file and n is the number
%% of processes to use

run([F]) ->
    run([F,"4"]);
run([FileName,NString]) ->
    N = list_to_integer(NString),
    main(FileName,N),
    halt().

%%-spec(main/2::([string(),integer()]) -> ok).

%% main can be used from the shell to do the same thing as
%% run

main(FileName,N) ->
    H = line_server:get_handle(FileName),
    Dict = merge_dicts(pmap(fun (F) ->
                                    loop(F,dict:new())
                            end, lists:duplicate(N,H))),
    line_server:close(H),
    List = dict:to_list(Dict),
    SortedList = lists:reverse(lists:keysort(2,List)),
    lists:foreach(fun pp/1, SortedList).

%%-spec(merge_dicts/1::([dict()]) -> dict()).

merge_dicts([D1,D2|Rest]) ->
    merge_dicts([dict:merge(fun(_,X,Y) -> X+Y end,D1,D2)|Rest]);
merge_dicts([D]) -> D.

%%-spec(loop/2::(handle(),dict()) -> dict()).

loop(Handle,Dict) ->
    case line_server:get_lines(Handle) of
        eof -> Dict;
        Lines ->
            loop(Handle,handle_lines(Lines,Dict))
    end.

%%-spec(pp/1::({binary(),integer()}) -> ok).

pp({Name,Gets}) ->
    io:format("~p get requests for ~s~n",[Gets,binary_to_list(Name)]).

%%-spec(handle_lines/2::([binary()], dict()) -> dict()).

handle_lines([Line|Rest],Dict) ->
    handle_lines(Rest,match_and_update_counter(Line,Dict));
handle_lines([],Dict) -> Dict.

%%-spec(match_and_update_counter/2::(binary(), dict()) -> dict()).

match_and_update_counter(Line, Dict) ->
    Word = get_nth_word(7,Line,$ ),
    case is_proper_get(Word) of
        true ->
            dict:update_counter(Word,1,Dict);
        false ->
            Dict
    end.

%%-spec(is_proper_get::(binary()) -> bool()).

is_proper_get(<<"/ongoing/When/",Rest/binary>>) ->
    not(member(Rest, $.));
is_proper_get(_) ->
    false.

%%-spec(member/2::(binary(),byte()) -> bool()).

member(Bin, X) ->
    case Bin of
        <<X,_/binary>> -> true;
        <<_,Rest/binary>> -> member(Rest, X);
        <<>> -> false
    end.

%%-spec(get_nth_word/3::(integer(), binary(), byte()) -> binary()).

get_nth_word(N,Bin,Val) ->
    get_word(0, skip_to_nth_word(N,Bin,Val),Val).

%%-spec(skip_to_nth_word/3::(integer(), binary(), byte()) -> binary()).

skip_to_nth_word(1,Bin,_Val) -> Bin;
skip_to_nth_word(N,Bin,Val) ->
    case Bin of
        <<Val,Rest/binary>> -> skip_to_nth_word(N-1,Rest,Val);
        <<_,Rest/binary>> -> skip_to_nth_word(N,Rest,Val)
    end.

%%-spec(get_word/3::(integer(), binary(), byte()) -> binary()).

get_word(S,Bin,Val) ->
    case Bin of
        <<_:S/binary,Val,_/binary>> ->
            <<Word:S/binary,_/binary>> = Bin,
            Word;
        _ ->
            get_word(S+1,Bin,Val)
    end.

%% pmap implementation taken from luke gorrie's blog
%% http://lukego.livejournal.com/6753.html

%%-spec(pmap/2::(fun((X) -> Y),[X]) -> [Y]).

pmap(F,List) ->
    [wait_result(Worker) || Worker <- [spawn_worker(self(),F,E) || E <- List]].

%%-spec(spawn_worker/3::(pid(),fun((X) -> any()),X) -> {pid(),ref()}).

spawn_worker(Parent, F, E) ->
    erlang:spawn_monitor(fun() -> Parent ! {self(), F(E)} end).

%%-spec(wait_result/1::({pid(),ref()}) -> any()).

wait_result({Pid,Ref}) ->
    receive
        {'DOWN', Ref, _, _, normal} -> receive {Pid,Result} -> Result end;
        {'DOWN', Ref, _, _, Reason} -> exit(Reason)
    end.

