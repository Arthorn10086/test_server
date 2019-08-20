-module(server_db_client).

%%%=======================STATEMENT====================
-description("server_db_client").
-author("yhw").

%%%=======================EXPORT=======================
-export([get/3, get/5, update/3, update/7, delete/2, delete/4]).

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
%%
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
%%
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
%%
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
%%
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
%%
%% @end
%% ----------------------------------------------------
%%transaction(TableKeys, MFA, Args) ->
%%    transaction(TableKeys, MFA, Args, ?LOCKTIMEOUT, ?TIMEOUT).
%%transaction(TableKeys, MFA, Args, LockTime, TimeOut) ->
%%    TableIdKeys = [{server_db_lib:get_db_name(TableName), Key, Default} || {TableName, Key, Default} <- TableKeys],
%%
%%
%%    ok.

%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%
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