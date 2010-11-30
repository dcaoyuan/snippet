-module(msg_queue).

-export([sync/2,
         async/2,
         proc_loop/4
        ]).

% 100000, 10
sync(N_Msg, N_Procs) ->
   test(N_Msg, N_Procs, fun disk_log:blog/2).

async(N_Msg, N_Procs) ->
   test(N_Msg, N_Procs, fun disk_log:balog/2).


test(N_Msg, N_Procs, FunLog) ->
   MsgPerProc = round(N_Msg / N_Procs),
   Collector = init(N_Procs),
   Workers = [spawn(?MODULE, proc_loop, [Collector, MsgPerProc, "", FunLog]) || _I <- lists:seq(1, N_Procs)],
   Start = now(),
   [Worker ! start || Worker <- Workers],
   %% don't terminate, wait here, until all tasks done.
   receive
      {sent_done, MaxMQLen, MaxMem} ->
	 probe_proc(Collector, MaxMQLen, MaxMem),
         io:format("Time: ~10.2f ms~n", [timer:now_diff(now(), Start) / 1000]),
         [exit(Worker, kill) || Worker <- Workers]
   end.

init(N_Procs) ->
   MainPid = self(),
   Collector = spawn(fun() -> collect(MainPid, N_Procs, 0, 0, 0, 0) end),
   Collector.

collect(MainPid, N_Procs, N_Finished, _N_Msg, MaxMQLen, MaxMem) when N_Procs == N_Finished ->
   MainPid ! {sent_done, MaxMQLen, MaxMem};
collect(MainPid, N_Procs, N_Finished, N_Msg, MaxMQLen, MaxMem) ->
   receive
      {Pid, sent_done, MQLen, _Mem} ->
         io:format("==== QLen ~p. Proc ~p finished, total finished: ~p ====~n", [MQLen, Pid, N_Finished + 1]),
         collect(MainPid, N_Procs, N_Finished + 1, N_Msg, MaxMQLen, MaxMem);
      {Pid, I, MQLen, Mem} ->
         MQLenSelf = mqlen(self()),
         MemSelf = mem(self()),
         io:format("Processed/Qlen ~p/~p msgs. mem is ~p. proc ~p: No.~p msgs sent~n", [N_Msg + 1, MQLenSelf, MemSelf, Pid, I]),
         MaxMQLen1 = if MQLen > MaxMQLen -> MQLen; true -> MaxMQLen end,
         MaxMem1 = if Mem > MaxMem -> Mem; true -> MaxMem end,
         collect(MainPid, N_Procs, N_Finished, N_Msg + 1, MaxMQLen1, MaxMem1);
      _ -> 
         io:format("========Unknown msgs", []),
	 collect(MainPid, N_Procs, N_Finished, N_Msg, MaxMQLen, MaxMem)  
   end.

proc_loop(Collector, N_Msg, LogPid, LogFun) ->
   receive
      start ->
         do_proc_work(Collector, N_Msg, LogPid, LogFun, do_log)
   end.

do_proc_work(Collector, I, LogPid, LogFun, WorkType) ->
   Date = httpd_util:rfc1123_date(calendar:local_time()),
   %erlang:garbage_collect(Collector),
   MQLen = mqlen(Collector),
   Mem = mem(Collector),
   Msg = io_lib:format("logged in ~p, qlen is ~p, total mem is ~p\n",
                       [self(), MQLen, Mem]),
   Msg1 = list_to_binary([<<"=INFO REPORT==== ">>, Date, <<" ===\n">>, Msg, <<"\n">>]),
   WorkType1 = if WorkType == do_log ->
                     io:format("", []), % sync the io between collector if any        
                     Collector ! {self(), I, MQLen, Mem},
                     io:format("sent one msg, qlen:~p, mem:~p~n", [MQLen, Mem]),
                     if I =< 1 ->
                           Collector ! {self(), sent_done, MQLen, Mem},
                           io:format("~p sent done, qlen:~p, mem:~p~n", [self(), MQLen, Mem]),
                           keep_live;
                        true -> do_log
                     end;
                  true -> keep_live
               end,
   do_proc_work(Collector, I - 1, LogPid, LogFun, WorkType1).
%   if WorkType == keep_live -> go;
%      true -> do_proc_work(Collector, I - 1, LogPid, LogFun, WorkType1)
%   end.

probe_proc(Pid, MaxMQLen, MaxMem) ->
   MQLen = mqlen(Pid),
   Mem = mem(Pid),
   MaxMQLen1 = if MQLen > MaxMQLen -> MQLen; true -> MaxMQLen end,
   MaxMem1 = if Mem > MaxMem -> Mem; true -> MaxMem end,
   io:format("qlen is ~p, max qlen is ~p, max mem is ~p~n", [MQLen, MaxMQLen1, MaxMem1]),
   if MQLen == 0 -> done;
      true ->
         timer:sleep(10),
         probe_proc(Pid, MaxMQLen, MaxMem)
   end.

%% === helper ===

mqlen(undefined) -> 0;
mqlen(Pid) ->
   case process_info(Pid, message_queue_len) of
      {message_queue_len, Val} when is_integer(Val) -> Val;
      _ -> 0
   end.

mem(undefined) -> 0;
mem(Pid) ->
   case process_info(Pid, memory) of
      {memory, Val} when is_integer(Val) -> Val;
      _ -> 0
   end.

msgs(undefined) -> 0;
msgs(Pid) ->
   case process_info(Pid, messages) of
      {messages, Val} -> Val;
      _ -> []
   end.

