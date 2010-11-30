%%% File    : wfinder4.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%% Created : 24 Oct 2007 by Anders Nygren <anders.nygren@gmail.com>

-module(wfinder10).
-compile([native]).

-export([start/2,start/3,
	 main/1
	]).

-define(BLOCKSIZE, 200000).

-record(state, {file, offset, fsize, blocksize, prev, rcv, 
		tid, tbl, ws}).

main([File]) ->
    start(File, ?BLOCKSIZE, erlang:system_info(schedulers)),
    halt();
main([File, Blksize]) ->
    start(File, list_to_integer(Blksize),erlang:system_info(schedulers)),
    halt();
main([File, Blksize, MaxWs]) when MaxWs>1 ->
    start(File, list_to_integer(Blksize), list_to_integer(MaxWs)),
    halt().

start(File, Blksize) ->
    start(File, Blksize, erlang:system_info(schedulers)).

start(File, Blksize, MaxWs) when MaxWs>0 ->
    process_flag(min_heap_size, 50000),
    Tid = ets:new(wftab, [set,public]),
    Rcv = start_receiver(Tid) ,
    Tbl = wfbm6:init(),
    {Offset, Prev} = start_workers(MaxWs, File, Blksize, Rcv, Tid, Tbl),
    main_loop(#state{file=File, offset=Offset, 
		     fsize=filelib:file_size(File), blocksize=Blksize,
		     prev=Prev, rcv=Rcv, tid=Tid, tbl=Tbl, ws=MaxWs}).
%%    io:format("~p~n",[statistics(garbage_collection)]).

main_loop(State) ->
    receive
 	Msg ->
%%	    trace_msg(Msg, State),
 	    case handle_msg(Msg, State) of
 		{continue, NState} ->
 		    main_loop(NState);
 		done ->
 		    done
 	    end
    end.

handle_msg(done, #state{rcv=Rcv, ws=1} = State) ->
    Rcv ! {done, self()},
    {continue, State#state{ws=0}};

handle_msg(done, #state{ws=Ws} = State) ->
    {continue, State#state{ws = Ws - 1}};

handle_msg({top_ten, L}, _State) ->
    lists:foreach(fun({K,V}) -> io:format("~p: ~s~n", [V, K]) end, L),
    done.


start_workers(MaxWs, File, Blksize, Rcv, Tid, Tbl) ->
    Size = filelib:file_size(File),
    Chunk = Size div MaxWs,
    lists:foldl(fun (N, {Offset, Prev}) when N == MaxWs ->
			Pid = start_worker(Prev, File, Offset, 
					   Size-Offset,
					   Blksize, Rcv, Tid, Tbl),
			Pid ! {tail,<<>>},
			{Offset + Blksize, Pid};
		    (_, {Offset, Prev}) ->
			Pid = start_worker(Prev, File, Offset, Chunk,
					   Blksize, Rcv, Tid, Tbl),
			{Offset + Chunk, Pid}
		end, {0, undefined}, lists:seq(1,MaxWs)).
    

start_worker(Prev, File, Offset, Len, Blksize, Rcv, Tid, Tbl) ->
    Me = self(),
    spawn_opt(fun() -> 
                      worker(Prev, File, Offset, Len, Blksize, 
                             Rcv, Tid, Tbl, Me)
              end,
              [{min_heap_size, 4000}]).

worker(Prev, File, Offset, Len, Blksize, Rcv, Tid, Tbl, AccTo) ->
    MyTid = ets:new(mytab, [set,public]),
    {ok,Fd} = file:open(File, [read,raw,binary]),
    {ok,Bin} = file:pread(Fd, Offset, Blksize),
    First = case Prev of
		undefined -> 0;
		_ -> 
		    Fst = split_first(Bin,0) + 1,
		    <<H:Fst/binary, _/binary>> = Bin,
		    Prev ! {tail, H},
		    Fst
	    end,
    Last = split_last(Bin, size(Bin)),
    Len1 =  Last - First,
    <<_:First/binary, Bin1:Len1/binary, _Tail/binary>> = Bin,
    process_chunk(Bin1, Tbl, Rcv, MyTid),
    Tail = process_chunks(Fd, Offset+Last, Offset+Len, Blksize,
			  Rcv, MyTid, Tbl),
    ok = file:close(Fd),
    receive
	{tail, B} ->
	    Rest = concat_binary([Tail,B]),
	    process_chunk(Rest, Tbl, Rcv, MyTid)
%%	    ,rendezvous()
    end,

    ets:foldl(fun ({Key,Val}=Match, Acc) -> 
		      case catch ets:update_counter(Tid, Key, Val) of
			  {'EXIT', _Rsn} -> 
			      Rcv ! Match;
			  _ -> ok
		      end,
		      Acc
	      end, undefined, MyTid),
    AccTo ! done.


process_chunks(Fd, Offset, To, Blksize, Rcv, Tid, Tbl) when Offset+Blksize<To ->
    {ok,Bin} = file:pread(Fd, Offset, Blksize),
    rendezvous(),
    Last = split_last(Bin, Blksize),
    <<Bin1:Last/binary, _/binary>> = Bin,
    process_chunk(Bin1, Tbl, Rcv, Tid),
    process_chunks(Fd, Offset+Last, To, Blksize, Rcv, Tid, Tbl);
process_chunks(Fd, Offset, To, _Blksize, Rcv, Tid, Tbl) ->
    {ok,Bin} = file:pread(Fd, Offset, To-Offset),
    rendezvous(),
    Last = split_last(Bin, size(Bin)),
    <<Bin1:Last/binary, Tail/binary>> = Bin,
    process_chunk(Bin1, Tbl, Rcv, Tid),
    rendezvous(),
    Tail.

process_chunk(Bin, Tab, Rcv, Tid) ->
    Me = self(),
    spawn_opt(fun() -> 
                      wfbm6:find(Me, Bin, Tab)
              end,
              [{min_heap_size, 10000}
               %%			,{fullsweep_after,100}
               %%			,{priority, high}
              ]).

rendezvous() ->
    receive
	done ->
	    continue
    end.

split_first(Bin, Offset) ->
    case Bin of
	<<_:Offset/binary,$\n,_/binary>> -> Offset;
	_ -> split_first(Bin, Offset + 1)
    end.

split_last(Bin, Offset) when Offset > 0 ->
    case Bin of
	<<_:Offset/binary,$\n,_/binary>> -> Offset;
	_ -> split_last(Bin, Offset - 1)
    end;
split_last(_, _) -> 0.

%%%==================================================================
%%% receive_matches
%%% Process that receives matches from the workers.
%%% This is only used the first time a specific match is found
%%% inorder to avoid a race condition on the insertion of the
%%% match in the ETS table.
%%%==================================================================
start_receiver(Tid) ->
    spawn_opt(fun() ->
		      receive_matches(Tid) 
	      end,
	      [{min_heap_size, 10000}]).

receive_matches(Tab) ->
    receive
	{done, Pid} ->
	    Pid ! {top_ten, top_ten(Tab)};
        {Key, Val}=Match -> 
%%	    case catch ets:update_counter(Tab, Match, 1) of
	    case catch ets:update_counter(Tab, Key, Val) of
		N when is_integer(N) ->
		    ok;
		_Error ->
		    ets:insert(Tab, Match)
%%		    ets:insert(Tab, {Match, 1})
	    end,
	    receive_matches(Tab)
    end.

top_ten(Tab) ->
    lists:reverse(ets:foldl(fun (M,Acc) when length(Acc) <10 ->
				    [M|Acc];
				({_,X}=E, [{_,C}|Es]) when X>C-> 
				    lists:keysort(2, [E|Es]);
				(_E, Acc) ->
				    Acc
			    end, [], Tab)).


%%%==================================================================
%%% Test
%%%==================================================================

trace_msg(done, #state{ws=Ws}) ->
    io:format("done ~p~n",[Ws]);
trace_msg({top_ten, L}, _State) ->
    io:format("top_ten ~p~n",[L]).
