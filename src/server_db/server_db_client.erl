-module(server_db_client).

%%%=======================STATEMENT====================
-description("server_db_client").
-author("yhw").

%%%=======================EXPORT=======================
-export([get/3, get/5, update/3, update/7, delete/2, delete/4, transaction/3, transaction/5, write/4]).

%%%=======================INCLUDE======================

%%%=======================DEFINE======================
-define(LOCKTIMEOUT, 7000).%%锁超时时间
-define(TIMEOUT, 5000).%%超时时间
-define(LOCK_INTERVAL, 50).%%超时时间
%%%=======================RECORD=======================

%%%=======================TYPE=========================
%%-type my_type() :: atom() | integer().


%%%=================EXPORTED FUNCTIONS=================
%% ----------------------------------------------------
%% @doc
%%      读
%% @end
%% ----------------------------------------------------
get(TableName, Key, Default) ->
    get(TableName, Key, Default, 0, ?TIMEOUT).
get(TableName, Key, Default, LockTime, TimeOut) ->
    STime = time_lib:now_millisecond(),
    ID = server_db_lib:get_db_name(TableName),
    SendInfo = format_send({'get', Key, LockTime}),
    case gen_server:call(ID, SendInfo, TimeOut) of
        'none' ->
            {'ok', Default, 0, 0};
        'lock_error' ->
            timer:sleep(?LOCK_INTERVAL),
            NMS = time_lib:now_millisecond(),
            NLockTime = LockTime - (STime - NMS),
            if
                NLockTime > 0 ->
                    get(TableName, Key, Default, NLockTime, TimeOut);
                true ->
                    throw({'lock_error', TableName, Key, LockTime})
            end;
        V ->
            V
    end.
%% ----------------------------------------------------
%% @doc
%%      修改
%% @end
%% ----------------------------------------------------
update(TableName, Key, Value) ->
    update(TableName, Key, fun(_, _) -> {'ok', 'ok', Value} end, 'none', [], ?LOCKTIMEOUT, ?TIMEOUT).
update(TableName, Key, Fun, Default, FunArgs, LockTime, TimeOut) ->
    ID = server_db_lib:get_db_name(TableName),
    {V, Vsn} = case get(TableName, Key, Default, LockTime, TimeOut) of
        {'ok', OldValue, Version, _Time} ->
            {OldValue, Version};
        {'error', Err} ->
            throw({'error', Err, TableName, Key})
    end,
    try
        case Fun(V, FunArgs) of
            {'ok', Return} ->
                Return;
            {'ok', Return, V} ->
                Return;
            {'ok', Return, NewValue} ->
                SendInfo = format_send({'update', Key, NewValue, Vsn}),
                gen_server:call(ID, SendInfo),
                Return;
            FunErr ->
                throw({'update_fun_error', 'bad_return', FunErr, TableName, Key, FunArgs})
        end
    catch
        throw:E1 ->
            E1;
        E2:E3 ->
            throw({'update_fun_error', E2, E3, TableName, Key, FunArgs, erlang:get_stacktrace()})
    after
        lock(TableName, Key, 0, ?TIMEOUT)
    end.

%% ----------------------------------------------------
%% @doc
%%      锁/LockTime 0 解锁
%% @end
%% ----------------------------------------------------
lock(TableName, Key, LockTime, TimeOut) ->
    ID = server_db_lib:get_db_name(TableName),
    STime = time_lib:now_millisecond(),
    SendInfo = format_send({'lock', Key, LockTime}),
    case catch gen_server:call(ID, SendInfo, TimeOut) of
        'ok' ->
            'ok';
        'lock_error' ->
            timer:sleep(?LOCK_INTERVAL),
            NMS = time_lib:now_millisecond(),
            NLockT = STime - NMS,
            if
                NLockT > 0 ->
                    lock(ID, Key, LockTime - (STime - time_lib:now_millisecond()), TimeOut);
                true ->
                    throw({'lock_error', ID, Key, NMS})
            end;
        Err ->
            throw({'lock_error', ID, Key, Err})
    end.
%% ----------------------------------------------------
%% @doc
%%      删除
%% @end
%% ----------------------------------------------------
delete(TableName, Key) ->
    delete(TableName, Key, ?LOCKTIMEOUT, ?TIMEOUT).
delete(TableName, Key, LockTime, TimeOut) ->
    ID = server_db_lib:get_db_name(TableName),
    case get(TableName, Key, 'none', LockTime, TimeOut) of
        {'ok', 'none', 0, 0} ->
            lock(TableName, Key, 0, ?TIMEOUT),
            ok;
        {'ok', _OldV, OldVsn, _} ->
            SendInfo = format_send({'delete', Key, OldVsn}),
            case catch gen_server:call(ID, SendInfo, TimeOut) of
                ok ->
                    lock(TableName, Key, 0, ?TIMEOUT);
                Err ->
                    lock(TableName, Key, 0, ?TIMEOUT),
                    throw({'delete_error', Err})
            end;
        {'error', Err} ->
            lock(TableName, Key, 0, ?TIMEOUT),
            throw({'error', Err, TableName, Key})
    end,
    ok.

%% ----------------------------------------------------
%% @doc
%%      写
%% @end
%% ----------------------------------------------------
write(TableName, Key, Value, Vsn) ->
    ID = server_db_lib:get_db_name(TableName),
    SendInfo = format_send({'update', Key, Value, Vsn}),
    gen_server:call(ID, SendInfo).
%% ----------------------------------------------------
%% @doc
%%      事务
%% @end
%% ----------------------------------------------------
transaction(TableKeys, MFA, Args) ->
    transaction(TableKeys, MFA, Args, ?LOCKTIMEOUT, ?TIMEOUT).
transaction(TableKeys, MFA, Args, LockTime, TimeOut) ->
    Ref = make_ref(),
    TableKeyL = [{TableName, Key} || {TableName, Key, _Default} <- TableKeys],
    transaction_send(TableKeys, Ref, LockTime, TimeOut),
    case transaction_receive(TableKeyL, Ref, []) of
        {ok, TableKeyVs} ->
            case catch transaction_(MFA, Args, TableKeys, TableKeyVs) of
                {'error', Err1} ->
                    throw({Err1, MFA, TableKeyVs, Args});
                Reply ->
                    try transaction_after(Reply, TableKeyVs) of
                        {'error', Err} ->
                            throw({Err, Reply, MFA, TableKeyVs, Args});
                        {ok, Msg} ->
                            Msg
                    catch
                        _E1:E2 ->
                            throw({'transaction_after', E2, Reply, MFA, TableKeyVs, Args})
                    after
                        transaction_unlock(TableKeyL)
                    end
            end;
        {'error', Error} ->
            throw({'lock_error', Error})
    end.

%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%      format
%% @end
%% ----------------------------------------------------
format_send({'get', Key, LockTime}) ->
    {'get', Key, 'ingore', 'ingore', LockTime, self()};
format_send({'lock', Key, LockTime}) ->
    {'lock', Key, 'ingore', 'ingore', LockTime, self()};
format_send({'update', Key, Value, Vsn}) ->
    {'update', Key, Value, Vsn, 0, self()};
format_send({'delete', Key, Vsn}) ->
    {'delete', Key, 'ingore', Vsn, 0, self()}.

%% ----------------------------------------------------
%% @doc
%%      事务辅助函数
%% @end
%% ----------------------------------------------------
transaction_send(TableKeys, Ref, LockTime, TimeOut) ->
    P = self(),
    lists:foreach(fun({TableName, Key, Default}) -> spawn(fun() ->
        case catch get(TableName, Key, Default, LockTime, TimeOut) of
            {ok, V, Vsn, _T} ->
                P ! {Ref, TableName, Key, V, Vsn};
            _ ->
                P ! {Ref, TableName, Key, 'lock_error'}
        end
    end) end, TableKeys).

transaction_receive(TableKeys, Ref, RcvL) ->
    receive
        {Ref, TableName, Key, V, Vsn} ->
            NRcvL = [{{TableName, Key}, V, Vsn} | RcvL],
            case lists:delete({TableName, Key}, TableKeys) of
                [] ->
                    {'ok', NRcvL};
                NL ->
                    transaction_receive(NL, Ref, NRcvL)
            end;
        {Ref, TableName, Key, 'lock_error'} ->
            {'error', {TableName, Key}};
        _ ->
            transaction_receive(TableKeys, Ref, RcvL)
    end.


transaction_(MFA, Args, TableKeys, TableKeyVs) ->
    TableKeyValues = lists:reverse(lists:foldl(fun({TableName, Key, _}, R) ->
        {_, V, _Vsn} = lists:keyfind({TableName, Key}, 1, TableKeyVs),
        [{{TableName, Key}, V} | R]
    end, [], TableKeys), []),
    case MFA of
        {M, F, A} ->
            M:F(A, Args, TableKeyValues);
        Fun when is_function(Fun, 2) ->
            Fun(Args, TableKeyValues);
        _ ->
            {'error', 'transaction_fun_error'}
    end.


transaction_after(Reply, TableKeyVs) ->
    case Reply of
        {ok, Msg} ->
            {ok, Msg};
        {ok, Msg, UpdateL} ->
            handle_update(TableKeyVs, UpdateL),
            {ok, Msg};
        {ok, Msg, UpdateL, AddL} ->
            handle_update(TableKeyVs, UpdateL),
            handle_add(AddL),
            {ok, Msg};
        _ ->
            {error, 'transaction_fun_bad_return'}
    end.

handle_update(TableKeyVs, UpdateL) ->
    lists:foreach(fun({{Tab, Key}, V}) ->
        {_, _, Vsn} = lists:keyfind({Tab, Key}, 1, TableKeyVs),
        write(Tab, Key, V, Vsn)
    end, UpdateL).
handle_add(AddL) ->
    lists:foreach(fun({Tab, Key, V}) ->
        write(Tab, Key, V, 0)
    end, AddL).
transaction_unlock(TableKeyL) ->
    lists:foreach(fun({TableName, Key}) ->
        lock(TableName, Key, 0, ?TIMEOUT)
    end, TableKeyL).