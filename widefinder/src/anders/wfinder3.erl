%%% File    : wfinder3.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%% Created : 24 Oct 2007 by Anders Nygren <anders.nygren@gmail.com>

-module(wfinder3).
-compile([native]).

-export([start/2,start/3,
	 main/1, worker/8
	]).

-define(BLOCKSIZE, 200000).

-record(state, {file, offset, fsize, blocksize, prev, rcv, 
		tid, tbl, ws}).

main([File]) ->
    start(File, ?BLOCKSIZE, erlang:system_info(schedulers)*2),
    halt();
main([File, Blksize]) ->
    start(File, list_to_integer(Blksize),erlang:system_info(schedulers)*2),
    halt();
main([File, Blksize, MaxWs]) when MaxWs>1 ->
    start(File, list_to_integer(Blksize), list_to_integer(MaxWs)),
    halt().

start(File, Blksize) ->
    start(File, Blksize, erlang:system_info(schedulers)*2).

start(File, Blksize, MaxWs) when MaxWs>1 ->
    process_flag(min_heap_size, 50000),
    Tid = ets:new(wftab, [set,public]),
    Rcv = start_receiver(Tid) ,
    Tbl = wfbm4_ets2:init(),
    {Offset, Prev} = start_workers(MaxWs, File, Blksize, Rcv, Tid, Tbl),
    main_loop(#state{file=File, offset=Offset, 
		     fsize=filelib:file_size(File), blocksize=Blksize,
		     prev=Prev, rcv=Rcv, tid=Tid, tbl=Tbl, ws=MaxWs}),
    io:format("~p~n",[statistics(garbage_collection)]).

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

handle_msg(done, #state{file=File, offset=Offset, fsize=Size, 
			blocksize=Blksize,
			prev=Prev, rcv=Rcv, tid=Tid,
			tbl=Tbl} = State) when Offset<Size ->
    Pid = start_worker(Prev, File, Offset, Blksize, Rcv, Tid, Tbl),
    NOffset = Offset+Blksize,
    case NOffset<Size of
	true -> nothing;
	false ->  Pid!{tail,<<>>}
    end,
    {continue, State#state{offset=NOffset, prev=Pid}};

handle_msg(done, #state{offset=Offset, fsize=Size, rcv=Rcv, ws=1} = State)
  when Offset>=Size ->
    Rcv!{done, self()},
    {continue, State#state{ws=0}};

handle_msg(done, #state{ws=Ws} = State) ->
    {continue, State#state{ws=Ws-1}};

handle_msg({top_ten, L}, _State) ->
    lists:foreach(fun({K,V}) -> io:format("~p: ~s~n", [V, K]) end, L),
    done.


start_workers(MaxWs, File, Blksize, Rcv, Tid, Tbl) ->
    lists:foldr(fun (_, {Offset, Prev}) ->
			Pid = start_worker(Prev, File, Offset, 
					   Blksize, Rcv, Tid, Tbl),
			{Offset+Blksize, Pid}
		end, {0, undefined}, lists:seq(1,MaxWs)).
    

start_worker(Prev, File, Offset, Blksize, Rcv, Tid, Tbl) ->
    Me = self(),
    proc_lib:spawn_opt(fun() -> 
			       worker(Prev, File, Offset, Blksize, 
				      Rcv, Tid, Tbl, Me)
		       end,
		       [{min_heap_size, 8000}]).
worker(Prev, File, Offset, Blksize, Rcv, Tid, Tbl, Me) ->
    {ok,Fd} = file:open(File, [read,raw,binary]),
    {ok,Bin} = file:pread(Fd, Offset, Blksize),
    ok = file:close(Fd),
    wfbm4_ets2:find(Prev, Bin, Tbl, Rcv, Me, Tid).

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
	      [{min_heap_size, 10000}]
	     ).

receive_matches(Tab) ->
    receive
	{done, Pid} ->
	    Pid!{top_ten, top_ten(Tab)};
        Match -> 
	    case catch ets:update_counter(Tab, Match, 1) of
		N when is_integer(N) ->
		    ok;
		_Error ->
		    ets:insert(Tab, {Match, 1})
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
