-module(tool_lib).

%%%=======================STATEMENT====================
-description("tool_lib").
-author("arthorn").

%%%=======================EXPORT=======================
-export([decompile/1, rpc_load_mod/2, pstack/1, gc_all/0, tc/2]).
-export([etop/0, etop_mem/0, etop_stop/0]).
-export([fprof/3, eprof_all/1, eprof/2]).
-export([scheduler_usage/0, scheduler_usage/1]).
-export([scheduler_stat/0, scheduler_stat/1]).
-export([trace/1, trace/2, trace_stop/0]).
-export([proc_mem/1, proc_mem/2, proc_mem_all/1]).

%%%=======================INCLUDE======================

%%%=======================DEFINE======================

%%%=======================RECORD=======================

%%%=======================TYPE=========================
%%-type my_type() :: atom() | integer().


%%%=================EXPORTED FUNCTIONS=================
%% ----------------------------------------------------
%% @doc  
%%        反编译
%% @end
%% ----------------------------------------------------
decompile(Mod) ->
    {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(code:which(Mod), [abstract_code]),
    io:format("~s~n", [erl_prettypr:format(erl_syntax:form_list(AC))]).
%% ----------------------------------------------------
%% @doc
%%        远程节点加载本地模块
%% @end
%% ----------------------------------------------------
rpc_load_mod(Node, Mod) ->
    {_, Binary, _} = code:get_object_code(Mod),
    FileName = rpc:call(Node, 'code', 'which', [Mod]),
    rpc:call(Node, 'code', 'load_binary', [Mod, FileName, Binary]).

%% ----------------------------------------------------
%% @doc
%%        查看进程栈   发现大量进程挂起，进程数过高，运行慢，hang住等问题用到
%% @end
%% ----------------------------------------------------
pstack(Reg) when is_atom(Reg) ->
    case whereis(Reg) of
        undefined -> undefined;
        Pid -> pstack(Pid)
    end;
pstack(Pid) ->
    io:format("~s~n", [element(2, process_info(Pid, backtrace))]).
%% ----------------------------------------------------
%% @doc
%% etop工具
%% 参数:
%%  node        atom       节点
%%  port        integer    The used port
%%  accumulate  boolean    Def:false   true 累计
%%  lines       integer    显示个数
%%  interval    integer    间隔时间 秒
%%  sort        runtime | reductions | memory | msg_q
%%  output      输出形式 graphical | text
%%  tracing     on | off  Def:on   etop使用erlang跟踪工具,因此节点上不能有其他跟踪，除非设置off
%%  setcookie   string    远程节点时使用  
%% @end
%% ----------------------------------------------------
%进程CPU占用排名
etop() ->
    spawn(fun() -> etop:start([{output, text}, {interval, 10}, {lines, 20}, {sort, reductions}]) end).

%进程Mem占用排名
etop_mem() ->
    spawn(fun() -> etop:start([{output, text}, {interval, 10}, {lines, 20}, {sort, memory}]) end).

%停止etop
etop_stop() ->
    etop:stop().

%% ----------------------------------------------------
%% @doc
%%      全部进程GC 进程内存过高时，来一发，看看是内存泄露还是gc不过来
%% @end
%% ----------------------------------------------------fe
gc_all() ->
    [erlang:garbage_collect(Pid) || Pid <- processes()].


%% ----------------------------------------------------
%% @doc
%%   对MFA 执行分析，会严重减缓运行，建议只对小量业务执行
%%   fprof 结果比较详细，能够输出热点调用路径
%% @end
%% ----------------------------------------------------
fprof(M, F, A) ->
    fprof:start(),
    fprof:apply(M, F, A),
    fprof:profile(),
    fprof:analyse(),
    fprof:stop().
%% ----------------------------------------------------
%% @doc
%% 对整个节点内所有进程执行eprof, eprof 对线上业务有一定影响,慎用!
%% 建议TimeoutSec<10s，且进程数< 1000，否则可能导致节点crash
%% 结果:
%% 输出每个方法实际执行时间（不会累计方法内其他mod调用执行时间）
%% 只能得到mod - Fun 执行次数 执行耗时
%% @end
%% ----------------------------------------------------
eprof_all(TimeoutSec) ->
    eprof(processes() -- [whereis(eprof)], TimeoutSec).

eprof(Pids, TimeoutSec) ->
    eprof:start(),
    eprof:start_profiling(Pids),
    timer:sleep(TimeoutSec),
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop().


%% ----------------------------------------------------
%% @doc
%%   统计下1s每个调度器CPU的实际利用率(因为有spin wait、调度工作, 可能usage 比top显示低很多)
%% @end
%% ----------------------------------------------------
scheduler_usage() ->
    scheduler_usage(1000).

scheduler_usage(RunMs) ->
    erlang:system_flag(scheduler_wall_time, true),
    Ts0 = lists:sort(erlang:statistics(scheduler_wall_time)),
    timer:sleep(RunMs),
    Ts1 = lists:sort(erlang:statistics(scheduler_wall_time)),
    erlang:system_flag(scheduler_wall_time, false),
    Cores = lists:map(fun({{I, A0, T0}, {I, A1, T1}}) ->
        {I, (A1 - A0) / (T1 - T0)} end, lists:zip(Ts0, Ts1)),
    {A, T} = lists:foldl(fun({{_, A0, T0}, {_, A1, T1}}, {Ai, Ti}) ->
        {Ai + (A1 - A0), Ti + (T1 - T0)} end, {0, 0}, lists:zip(Ts0, Ts1)),
    Total = A/T,
    io:format("~p~n", [[{total, Total} | Cores]]).


%% ----------------------------------------------------
%% @doc
%%  统计下1s内调度进程数量(含义：第一个数字执行进程数量，第二个数字迁移进程数量)
%% @end
%% ----------------------------------------------------
scheduler_stat() ->
    scheduler_stat(1000).

scheduler_stat(RunMs) ->
    erlang:system_flag(scheduling_statistics, enable),
    Ts0 = erlang:system_info(total_scheduling_statistics),
    timer:sleep(RunMs),
    Ts1 = erlang:system_info(total_scheduling_statistics),
    erlang:system_flag(scheduling_statistics, disable),
    lists:map(fun({{Key, In0, Out0}, {Key, In1, Out1}}) ->
        {Key, In1 - In0, Out1 - Out0} end, lists:zip(Ts0, Ts1)).


%% ----------------------------------------------------
%% @doc
%%  trace日志
%% @end
%% ----------------------------------------------------
%trace Mod 所有方法的调用
trace(Mod) ->
    dbg:tracer(),
    dbg:tpl(Mod, '_', []),
    dbg:p(all, c).

%trace Node上指定 Mod 所有方法的调用, 结果将输出到本地shell
trace(Node, Mod) ->
    dbg:tracer(),
    dbg:n(Node),
    dbg:tpl(Mod, '_', []),
    dbg:p(all, c).

%停止trace
trace_stop() ->
    dbg:stop_clear().

%% ----------------------------------------------------
%% @doc
%%  内存高OOM 排查工具
%%  etop 无法应对10w+ 进程节点, 下面代码就没问题了,找到可疑proc后通过pstack、message_queu_len 排查原因
%% @end
%% ----------------------------------------------------
proc_mem_all(SizeLimitKb) ->
    Procs = [{undefined, Pid} || Pid <- erlang:processes()],
    proc_mem(Procs, SizeLimitKb).

proc_mem(SizeLimitKb) ->
    Procs = [{Name, Pid} || {_, Name, Pid, _} <- release_handler_1:get_supervised_procs(),
        is_process_alive(Pid)],
    proc_mem(Procs, SizeLimitKb).

proc_mem(Procs, SizeLimitKb) ->
    SizeLimit = SizeLimitKb * 1024,
    {R, Total} = lists:foldl(fun({Name, Pid}, {Acc, TotalSize}) ->
        case erlang:process_info(Pid, total_heap_size) of
            {_, Size0} ->
                Size = Size0 * 8,
                case Size > SizeLimit of
                    true -> {[{Name, Pid, Size} | Acc], TotalSize + Size};
                    false -> {Acc, TotalSize}
                end;
            _ -> {Acc, TotalSize}
        end
    end, {[], 0}, Procs),
    R1 = lists:keysort(3, R),
    {Total, lists:reverse(R1)}.

%% ----------------------------------------------------
%% @doc
%%      执行时间
%% @end
%% ----------------------------------------------------
tc(MFA, Times) ->
    statistics(wall_clock),
    tc_(MFA, Times),
    {_, Time} = statistics(wall_clock),
    Time / Times.

tc_(_, 0) ->
    ok;
tc_({M, F, A}, N) ->
    apply(M, F, A),
    tc_({M, F, A}, N - 1);
tc_(Fun, N) ->
    Fun(),
    tc_(Fun, N - 1).


%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
