%%% File    : wfinder.erl
%%% Author  : Anders Nygren <anders.nygren@gmail.com>
%%% Description : 
%%% Created : 24 Oct 2007 by Anders Nygren <anders.nygren@gmail.com>

-module(wfinder1_1).
-compile([native]).

-export([start/2,start/3,
	 main/1
	]).

-export([file_reader/3,top_ten/1, split_and_find/3]).

-define(BLOCKSIZE, 200000).

-record(state, {eof=false, rcv, tid, tbl, max_w, ws=0, data=[]}).

main([File]) ->
    start(File, ?BLOCKSIZE, erlang:system_info(schedulers)*2),
    halt();
main([File, Blksize]) ->
    start(File, list_to_integer(Blksize),erlang:system_info(schedulers)*2),
    halt();
main([File, Blksize, MaxWs]) ->
    start(File, list_to_integer(Blksize), list_to_integer(MaxWs)),
    halt().

start(File, Blksize) ->
    start(File, Blksize, erlang:system_info(schedulers)*2).

start(File, Blksize, MaxWs) ->
    process_flag(min_heap_size, 50000),
    Me = self(),
    start_reader(Me, File, Blksize),
    Tid = ets:new(wftab, [set,public]),
    Rcv = start_receiver(Tid) ,
    Tbl = wfbm4_ets1_1:init(),
    main_loop(#state{rcv=Rcv, tid=Tid, tbl=Tbl, max_w=MaxWs}).
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


handle_msg({data, Bin, Size}, 
 	   #state{max_w=MaxWs, ws=MaxWs, data=Bins} = State) ->
    {continue, State#state{data=[{Bin, Size}|Bins]}};

handle_msg({data, Bin, Size}, 
 	   #state{rcv=Rcv, tid=Tid, tbl=Tbl, ws=Ws, data=[]} = State) ->
    start_worker(Bin, Size, Rcv, Tid, Tbl),
    {continue, State#state{ws=Ws+1}};

handle_msg(eof, #state{rcv=Rcv, ws=0} = State) ->
    Rcv!{done, self()},
    {continue, State#state{eof=true}};

handle_msg(eof, State) ->
    {continue, State#state{eof=true}};

handle_msg(done, #state{eof=true, rcv=Rcv, ws=1, data=[]} = State) ->
    Rcv!{done, self()},
    {continue, State#state{ws=0}};

handle_msg(done, 
 	   #state{rcv=Rcv, tid=Tid, tbl=Tbl, ws=Ws, 
 		  data=[{Bin, Size}|Bins]} = State) ->
    start_worker(Bin, Size, Rcv, Tid, Tbl),
    {continue, State#state{ws=Ws, data=Bins}};

handle_msg(done, #state{ws=Workers, data=[]} = State) ->
    {continue, State#state{ws=Workers-1}};

handle_msg({top_ten, L}, _State) ->
    lists:foreach(fun({K,V}) -> io:format("~p: ~s~n", [V, K]) end, L),
    done.

start_worker(Bin, Size, Rcv, Tid, Tbl) ->
    Me = self(),
    proc_lib:spawn_opt(fun() -> 
			       wfbm4_ets1_1:find(Bin, Size, Tbl, Rcv, Me, Tid)
		       end,
		       [{min_heap_size, 8000}]).

%%%==================================================================
%%% file reader
%%% Process to read and chunk the file. Sends the chunks to the
%%% coordination process.
%%%==================================================================
start_reader(Me, File, Blksize) ->
    spawn(fun() -> 
		  file_reader(Me, File, Blksize)
%%  		  {T,_} = timer:tc(?MODULE,file_reader,[Me, File, Blksize]),
%%  		  io:format("read file ~p~n",[T])
	  end).

file_reader(Coord, File, Blksize) ->
    process_flag(min_heap_size, 8000),
    {ok, FD} = file:open(File, [read, raw, binary]),
    {ok, Bin} = file:read(FD, Blksize),
    scan_file(Coord, FD, Bin, Blksize),
    ok = file:close(FD).

scan_file(Coord, FD, Bin, Blksize) ->
    scan_file(Coord, FD, Bin, Blksize, Blksize).

scan_file(Coord, FD, Bin, Blksize, Offset) ->
    Next = split_and_find(Coord, Bin, Blksize),
%%     {T,Next} = timer:tc(?MODULE, split_and_find,[Coord, Bin, Blksize]),
%%     io:format("~p~n",[T]),
    Offset1 = Offset - (Blksize - Next),
    case file:pread(FD, Offset1, Blksize) of
	{ok, Block} ->
	    scan_file(Coord, FD, Block, Blksize, Offset1+Blksize);
	eof ->
	    Coord!eof
    end.

split_and_find(Coord, Bin, Blksize) ->
    Size = Blksize - 1,
    case Bin of
        <<_:Size/binary, $\n, _/binary>> ->
	    Coord!{data, Bin, Size},
	    Blksize;
        _ ->
            split_and_find(Coord, Bin, Size)
    end.

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

trace_msg({data, _Bin}, #state{ws=Ws, data=D}) ->
    io:format("data ~p ~p~n",[Ws, length(D)]);
trace_msg(done, #state{ws=Ws, data=D}) ->
    io:format("done ~p ~p~n",[Ws, length(D)]);
trace_msg(eof, #state{data=D}=State) ->
    L = length(D),
    NS= State#state{data=L, tbl=none},
    io:format("eof ~p~n",[NS]);
trace_msg({top_ten, L}, _State) ->
    io:format("top_ten ~p~n",[L]).
