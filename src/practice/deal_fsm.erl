-module(deal_fsm).
-author("yhw,yanghaiwei@youkia.net").

-behaviour(gen_fsm).

%% API
-export([start_link/1, handle_goods/3]).

%% gen_fsm callbacks
-export([init/1,
    normal/2,
    normal/3,
    consult/2,
    consult/3,
    ready/2,
    ready/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4]).

-define(SERVER, ?MODULE).
-define(APPLY_TIME, 60).%%申请超时时间

-record(state, {ruid, apply, goods = [], tar_uid}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(RUid) ->
    gen_fsm:start_link(?MODULE, [RUid], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([RUid]) ->
    {ok, normal, #state{ruid = RUid}}.


normal(_Event, State) ->
    {next_state, normal, State}.
consult(_Event, State) ->
    {next_state, consult, State}.
ready(_Event, State) ->
    {next_state, ready, State}.

normal({'apply', TarUid, Sec}, _From, State) ->
    {reply, ok, normal, State#state{apply = {TarUid, Sec + ?APPLY_TIME}}};
%接受交易申请
normal({'agree', TarUid, TarPid}, _From, #state{ruid = Uid} = State) ->
    case gen_fsm:sync_send_event(TarPid, {'agree', Uid}) of
        ok ->
            {reply, ok, consult, State#state{tar_uid = TarUid}};
        Error ->
            {reply, Error, normal, State}
    end;
%交易申请被接受
normal({'agree', TarUid}, _From, #state{apply = {ApplyID, EndTime}} = State) when is_integer(TarUid) ->
    Sec = time_lib:now_second(),
    if
        ApplyID =/= TarUid ->
            {reply, 'Invalid', normal, State};
        Sec > EndTime ->
            {reply, 'Invalid', normal, State};
        true ->
            {reply, ok, consult, State#state{tar_uid = TarUid}}
    end;
normal(_Event, _From, State) ->
    {reply, state_error, normal, State}.

%协商商品
consult({'consult', ChangeGoods}, _From, #state{ruid = Uid, goods = GoodsL, tar_uid = TarUid} = State) ->
    try
        NGoodsL = handle_goods(GoodsL, Uid, ChangeGoods),
        TarPid = deal_manager:get_deal_pid(TarUid),
        case gen_fsm:sync_send_event(TarPid, {'tar_consult', Uid, ChangeGoods}) of
            ok ->
                {reply, ok, consult, State#state{goods = NGoodsL}};
            _ ->%有内鬼,交易终止
                {reply, cancel, normal, #state{ruid = Uid}}
        end
    catch
        throw: Err ->
            {reply, Err, consult, State};
        _E1:Error ->
            {stop, Error, State}
    end;
consult({'tar_consult', TarUid, ChangeGoods}, _From, #state{tar_uid = TarUid, goods = GoodsL} = State) ->
    NGoodsL = handle_goods(GoodsL, TarUid, ChangeGoods),
    %推送 ChangeGoods
    {reply, ok, consult, State#state{goods = NGoodsL}};

%%准备
consult({'ready'}, _From, #state{ruid = RUid, tar_uid = TarUid, goods = Goods} = State) ->
    TarPid = deal_manager:get_deal_pid(TarUid),
    case gen_fsm:sync_send_event(TarPid, {'ready', RUid}) of
        'py_over' ->%达成PY交易
            handle_deal(Goods),
            {reply, {py_over, TarUid}, normal, #state{ruid = RUid}};
        _ ->
            {reply, ok, ready, State}
    end;
consult(_Event, _From, State) ->
    {reply, state_error, consult, State}.

%%已经准备,对方改变交易项目,取消准备状态
ready({'tar_consult', TarUid, ChangeGoods}, _From, #state{tar_uid = TarUid, goods = GoodsL} = State) ->
    NGoodsL = handle_goods(GoodsL, TarUid, ChangeGoods),
    %推送 NGoodsL
    {reply, ok, consult, State#state{goods = NGoodsL}};
%%取消准备
ready({'cancel_ready'}, _From, #state{ruid = RUid, tar_uid = TarUid} = State) ->
    TarPid = deal_manager:get_deal_pid(TarUid),
    gen_fsm:sync_send_event(TarPid, {'cancel_ready', RUid}),
    %推送 TarUid
    {reply, ok, consult, State};

%%对方准备完成,我方也已准备,执行交易事物
ready({'ready', TarUid}, _From, #state{ruid = RUid, tar_uid = TarUid}) ->
    {reply, py_over, normal, #state{ruid = RUid}};
ready(_Event, _From, State) ->
    {reply, state_error, ready, State}.

%%退出交易
handle_event({'deal_exit', _Uid}, normal, State) ->
    {next_state, normal, State};
handle_event({'deal_exit', TarUid}, _StateName, #state{ruid = RUid, tar_uid = TarUid}) ->
    {next_state, normal, #state{ruid = RUid}};

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.


handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.


handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.


terminate(Reason, _StateName, _State) ->
    io:format("=~p~n", [{?MODULE, ?LINE, Reason, _State, erlang:get_stacktrace()}]),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_goods(GoodsL, Uid, {Key, Num}) ->
    Goods1 = case lists:keyfind(Uid, 1, GoodsL) of
        {_, Goods} ->
            Goods;
        false ->
            []
    end,
    Goods2 = case lists:keytake(Key, 1, Goods1) of
        false ->
            Len = length(Goods1),
            if
                Num =< 0 ->
                    throw("num_error");
                Len >= 10 ->
                    throw("max_count");
                true ->
                    [{Key, Num} | Goods1]
            end;
        {_, {_, OldN}, T} ->
            if
                OldN + Num < 0 ->
                    throw("num_error");
                true ->
                    [{Key, Num + OldN} | T]
            end
    end,
    lists:keystore(Uid, 1, GoodsL, {Uid, Goods2}).


handle_deal(Goods) ->
    io:format("=~p~n", [{?MODULE, ?LINE, Goods}]).