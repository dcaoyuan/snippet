-module(dlogger).

-export([sync/2,
         async/2,
         proc_loop/4
        ]).

-define(LogName, blog).
-define(LogFile, "b.log").

% 100000, 10
sync(N_Msg, N_Procs) ->
    test(N_Msg, N_Procs, fun disk_log:blog/2).

async(N_Msg, N_Procs) ->
    test(N_Msg, N_Procs, fun disk_log:balog/2).


test(N_Msg, N_Procs, FunLog) ->
    MsgPerProc = round(N_Msg / N_Procs),
    Collector = init(N_Procs),
    LogPid = logger_pid(?LogName),
    io:format("logger pid: ~p~n", [LogPid]),
    Workers = [spawn(?MODULE, proc_loop, [Collector, MsgPerProc, LogPid, FunLog]) || _I <- lists:seq(1, N_Procs)],
    Start = now(),
    [Worker ! start || Worker <- Workers],
   %% don't terminate, wait here, until all tasks done.
    receive
        {sent_done, MaxMQLen, MaxMem} ->
            probe_logger(LogPid, MaxMQLen, MaxMem),
            [exit(Worker, kill) || Worker <- Workers],
            io:format("Time: ~10.2f ms~n", [timer:now_diff(now(), Start) / 1000])
    end.

init(N_Procs) ->
    disk_log:close(?LogName),
    disk_log:open([{name, ?LogName},
                   {file, ?LogFile},
                   {format, external}]),
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
         %io:format("Processed/Qlen ~p/~p msgs. Logger mem is ~p. proc ~p: No.~p msgs sent~n", [N_Msg + 1, MQLen, Mem, Pid, I]),
            MaxMQLen1 = if MQLen > MaxMQLen -> MQLen; true -> MaxMQLen end,
            MaxMem1 = if Mem > MaxMem -> Mem; true -> MaxMem end,
            collect(MainPid, N_Procs, N_Finished, N_Msg + 1, MaxMQLen1, MaxMem1)
    end.

proc_loop(Collector, N_Msg, LogPid, LogFun) ->
    receive
        start ->
            do_proc_work(Collector, N_Msg, LogPid, LogFun, do_log)
    end.

do_proc_work(Collector, I, LogPid, LogFun, WorkType) ->
    Date = httpd_util:rfc1123_date(calendar:local_time()),
    MQLen = logger_mqlen(LogPid),
    Mem = logger_mem(LogPid),
    Msg = io_lib:format("logged in ~p, logger qlen is ~p, total mem is ~p\n",
                        [self(), MQLen, Mem]),
    Msg1 = list_to_binary([<<"=INFO REPORT==== ">>, Date, <<" ===\n">>, Msg, <<"\n">>]),

    WorkType1 = if WorkType == do_log ->
            io:format("", []), % sync the io between collector if any
            LogFun(?LogName, Msg1),
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

probe_logger(Pid, MaxMQLen, MaxMem) ->
    MQLen = logger_mqlen(Pid),
    Mem = logger_mem(Pid),
    MaxMQLen1 = if MQLen > MaxMQLen -> MQLen; true -> MaxMQLen end,
    MaxMem1 = if Mem > MaxMem -> Mem; true -> MaxMem end,
    io:format("qlen is ~p, max qlen is ~p, max mem is ~p~n", [MQLen, MaxMQLen1, MaxMem1]),
    if MQLen == 0 ->
            done;
        true ->
            timer:sleep(10),
            probe_logger(Pid, MaxMQLen, MaxMem)
    end.

%% === helper ===
logger_pid(Log) ->
    case disk_log_server:get_log_pids(Log) of
        undefined ->
            undefined;
        {local, Pid} ->
            Pid;
        {distributed, [Pid|_Pids]} ->
            Pid
    end.

logger_mqlen(undefined) -> 0;
logger_mqlen(Pid) ->
    case process_info(Pid, message_queue_len) of
        {message_queue_len, Val} when is_integer(Val) -> Val;
        _ -> 0
    end.

logger_mem(undefined) -> 0;
logger_mem(Pid) ->
    case process_info(Pid, memory) of
        {memory, Val} when is_integer(Val) -> Val;
        _ -> 0
    end.

