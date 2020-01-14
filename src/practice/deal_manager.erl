-module(deal_manager).%交易管理器
-author("yhw,yanghaiwei@youkia.net").

-behaviour(gen_server).

%% API
-export([start_link/0, get_deal_pid/1, apply/2, agree/2, consult/2, ready/1, quit/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-export([test/0]).
-define(SERVER, ?MODULE).

-record(state, {ets}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%发起申请
apply(RUid1, RUid2) ->
    Pid1 = get_deal_pid(RUid1),
    Sec = time_lib:now_second(),
    case gen_fsm:sync_send_event(Pid1, {'apply', RUid2, Sec}) of
        ok ->
            %%通知对方玩家
            ok;
        Error ->
            Error
    end.
%同意申请
agree(RUid1, RUid2) ->
    Pid1 = get_deal_pid(RUid1),
    Pid2 = get_deal_pid(RUid2),
    case gen_fsm:sync_send_event(Pid1, {'agree', RUid2, Pid2}) of
        ok ->
            ets:insert(?MODULE, {{RUid1, 'mapping'}, RUid2}),
            ets:insert(?MODULE, {{RUid2, 'mapping'}, RUid1}),
            %%通知RUid2玩家
            ok;
        Err ->
            Err
    end.

%协商物品
consult(Uid, Goods) ->
    Pid1 = get_deal_pid(Uid),
    case gen_fsm:sync_send_event(Pid1, {'consult', Goods}) of
        ok ->
            ok;
        Err ->
            Err
    end.
%准备
ready(Uid) ->
    Pid1 = get_deal_pid(Uid),
    case gen_fsm:sync_send_event(Pid1, {'ready'}) of
        ok ->
            ok;
        {py_over, TarUid} ->
            ets:delete(?MODULE, {Uid, 'mapping'}),
            ets:delete(?MODULE, {TarUid, 'mapping'});
        Err ->
            Err
    end.
%取消交易
quit(Uid) ->
    Pid = get_deal_pid(Uid),
    case ets:lookup(?MODULE, {Uid, 'mapping'}) of
        [] ->
            "input_error";
        [{_, Uid1}] ->
            gen_fsm:send_all_state_event(Pid, {'deal_exit', Uid1}),
            Pid1 = get_deal_pid(Uid1),
            gen_fsm:send_all_state_event(Pid1, {'deal_exit', Uid}),
            ets:delete(?MODULE, {Uid, 'mapping'}),
            ets:delete(?MODULE, {Uid1, 'mapping'}),
            ok
    end.
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    Ets = ets:new(?MODULE, [named_table, set, public, {write_concurrency, true}, {read_concurrency, true}]),
    {ok, #state{ets = Ets}}.

handle_call({create, RUid}, _From, State) ->
    {ok, Pid} = deal_fsm:start_link(RUid),
    ets:insert(?MODULE, {RUid, Pid}),
    ets:insert(?MODULE, {Pid, RUid}),
    {reply, Pid, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({'EXIT', Pid, _Reason}, State) ->
    [{_, Uid}] = ets:lookup(?MODULE, Pid),
    ets:delete(?MODULE, Pid),
    ets:delete(?MODULE, Uid),
    case ets:lookup(?MODULE, {Uid, 'mapping'}) of
        [{_, Uid1}] ->
            gen_fsm:send_all_state_event(get_deal_pid(Uid1), {'deal_exit', Uid}),
            ets:delete(?MODULE, {Uid, mapping}),
            ets:delete(?MODULE, {Uid1, mapping}),
            ok;
        _ ->
            ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    io:format("=~p~n", [{?MODULE, ?LINE, _Info}]),
    {noreply, State}.


terminate(_Reason, _State) ->
    io:format("=~p~n", [{?MODULE, ?LINE, _Reason}]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%获得玩家进程
get_deal_pid(RUid) ->
    case ets:lookup(?MODULE, RUid) of
        [] ->
            Pid = gen_server:call(?MODULE, {create, RUid}),
            Pid;
        [{_, Pid}] ->
            Pid
    end.

%%%===================================================================
%%% Test functions
%%%===================================================================
test() ->
    deal_manager:start_link(),
    deal_manager:apply(1, 2),
    deal_manager:agree(2, 1),
    deal_manager:quit(1),
    deal_manager:apply(2, 1),
    deal_manager:agree(2, 1),
    deal_manager:agree(1, 2),
    deal_manager:consult(1, {rmb, 100}),
    deal_manager:ready(1),
    deal_manager:consult(2, {mb, 200}),
    deal_manager:ready(2),
    deal_manager:ready(1),
    ok.