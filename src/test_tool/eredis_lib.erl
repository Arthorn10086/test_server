-module(eredis_lib).

%%%=======================STATEMENT====================
-description("eredis_lib").
-author("arthorn").
-vsn(1.0).

-import(db_lib, [term_to_string/1, string_to_term/1, now_millisecond/0, now_second/0]).
%%%=======================EXPORT=======================
-export([sync_command/2, sync_command/3, sync_command/4, async_command/2, async_command/3]).
-export([lock/1, lock/2, lock/3, lock/4, unlock/1]).
-export([getv/1, getv/2, gets/1, gets1/1]).
-export([delete/1, delete1/1]).
-export([dirty_write/2, dirty_write/3, dirty_write/4]).
-export([update/5, update/6, transaction/4]).
-export([redis_str_key/1, redis_str_key/2]).
-export([redis_lock_key/1, redis_lock_key/2]).
%%%=======================INCLUDE======================

%%%=======================DEFINE======================
-define(POOLNAME, dbsrv).
-define(TIMEOUT, 5000).
-define(LOCKTIME, 5).
%%%=======================RECORD=======================

%%%=======================TYPE=========================
%%-type my_type() :: atom() | integer().


%%%=================EXPORTED FUNCTIONS=================
%% ----------------------------------------------------
%% @doc  
%%       同步执行命令
%% @end
%% ----------------------------------------------------
sync_command(Order, Params) ->
    sync_command(?POOLNAME, Order, Params).
sync_command(PoolName, Order, Params) ->
    sync_command(PoolName, Order, Params, ?TIMEOUT).
sync_command(PoolName, Order, Params, TimeOut) ->
    eredis_pool:q(PoolName, [Order | Params], TimeOut).
%% ----------------------------------------------------
%% @doc
%%       异步执行命令
%% @end
%% ----------------------------------------------------
async_command(Order, Params) ->
    async_command(?POOLNAME, Order, Params).
async_command(PoolName, Order, Params) ->
    eredis_pool:q_async(PoolName, [Order | Params]).
%% ----------------------------------------------------
%% @doc
%%       获取 str类型的值
%% @end
%% ----------------------------------------------------
getv(TableKey) ->
    getv(TableKey, 'none').
getv(TableKey, Def) ->
    case sync_command("GET", [TableKey]) of
        {error, Err} ->
            throw(Err);
        {ok, undefined} ->
            Def;
        {ok, B} ->
            B
    end.

delete(TableKey) ->
    sync_command("DEL", [TableKey]).
delete1(TableKey) ->
    R = getv(TableKey),
    delete(TableKey),
    R.
%% ----------------------------------------------------
%% @doc
%%       获取 str类型的值
%% @end
%% ----------------------------------------------------
gets(TableKeyDefs) ->
    {TableKeys, Defs} =
        lists:foldl(fun({T, K}, {R1, R2}) ->
            {[{T, K} | R1], ['ignore' | R2]};
            ({T, K, D}, {R1, R2}) ->
                {[{T, K} | R1], [D | R2]}
        end, {[], []}, TableKeyDefs),
    Values = gets1(redis_str_key(TableKeys)),
    lists:map(fun({V, DV}) ->
        if
            DV =:= 'ignore' ->
                V;
            V =:= undefined ->
                DV;
            true ->
                V
        end
    end, lists:zip(Values, Defs)).

gets1(TableKeys) ->
    element(2,sync_command("MGET", TableKeys)).
%% ----------------------------------------------------
%% @doc
%%       脏写
%% @end
%% ----------------------------------------------------
dirty_write(TableKey, Value) ->
    sync_command("SET", [TableKey, Value]).
dirty_write(TableKey, Value, EX) when is_integer(EX) ->
    sync_command("SET", [TableKey, Value, "EX", EX]);
dirty_write(TableKey, Value, NX) when is_boolean(NX) ->
    if
        NX ->
            sync_command("SET", [TableKey, Value, "NX"]);
        true ->
            dirty_write(TableKey, Value)
    end.
dirty_write(TableKey, Value, NX, EX) ->
    if
        NX ->
            sync_command("SET", [TableKey, Value, "EX", EX, "NX"]);
        true ->
            dirty_write(TableKey, Value, EX)
    end.

%% ----------------------------------------------------
%% @doc
%%       锁  LockTime:锁超时时间  TimeOut:客户端等待超时时间
%% @end
%% ----------------------------------------------------
lock(Table, Key) ->
    lock(Table, Key, ?LOCKTIME, ?TIMEOUT).
lock(Table, Key, LockTime, TimeOut) ->
    LockKey = redis_lock_key(Table, Key),
    lock(LockKey, LockTime, TimeOut).

lock(LockKey) ->
    lock(LockKey, ?LOCKTIME, ?TIMEOUT).
lock(LockKey, LockTime, TimeOut) ->
    lock_(LockKey, term_to_string(self()), LockTime, TimeOut).

lock_(SLKey, V, _LockTime, TimeOut) when TimeOut =< 0 ->
    throw({lock_error, SLKey, V});
lock_(SLKey, V, LockTime, TimeOut) ->
    Ms1 = now_millisecond(),
    case sync_command("SET", [SLKey, V, "EX", LockTime, "NX"]) of
        {ok, <<"OK">>} ->
            ok;
        _ ->
            MS2 = now_millisecond(),
            lock_(SLKey, V, LockTime, TimeOut - (MS2 - Ms1))
    end.
%% ----------------------------------------------------
%% @doc
%%       解除锁
%% @end
%% ----------------------------------------------------
unlock([{_Table, _Key} | _T] = TableKeys) ->
    LockKeys = redis_lock_key(TableKeys),
    unlock(LockKeys);
unlock(LockKeys) ->
    V = list_to_binary(term_to_string(self())),
    sync_command("WATCH", LockKeys),
    {ok, VL} = sync_command("MGET", LockKeys),
    sync_command("MULTL", []),
    lists:foreach(fun({LockKey, V1}) ->
        case V =:= V1 of
            true ->%%只能删除自己的锁
                sync_command("DEL", [LockKey]);
            false ->
                ok
        end
    end, lists:zip(LockKeys, VL)),
    sync_command("EXEC", []),
    sync_command("UNWATCH", []),
    ok.
%% ----------------------------------------------------
%% @doc
%%       锁修改
%% @end
%% ----------------------------------------------------
update(Table, Key, Fun, Def, Args) ->
    update(Table, Key, Fun, Def, Args, ?TIMEOUT).
update(Table, Key, Fun, Def, Args, TimeOut) ->
    LockKey = redis_lock_key(Table, Key),
    lock(LockKey, ?LOCKTIME, TimeOut),
    TableKey = redis_str_key(Table, Key),
    OldV = getv(TableKey, Def),
    try
        case Fun(Args, OldV) of
            {ok, Reply} ->
                Reply;
            {ok, Reply, OldV} ->
                Reply;
            {ok, Reply, NV} ->
                dirty_write(TableKey, NV),
                Reply;
            Other ->
                throw({?MODULE, ?FUNCTION_NAME, 'bad_return', Other})
        end
    catch
        _EType:Reason ->
            Reason
    after
        unlock([LockKey])
    end.
%% ----------------------------------------------------
%% @doc
%%       事务
%% @end
%% ----------------------------------------------------
transaction(TableKeyDefs, F, Args, TimeOut) ->
    TableKeys = [redis_lock_key(T, K) || {T, K, _D} <- TableKeyDefs],
    LockKeys = transaction_locks(TableKeys, TimeOut, term_to_string(self())),
    Values = gets(TableKeyDefs),
    Reply1 = case catch F(Args, Values) of
        {ok, Reply} ->
            Reply;
        {ok, Reply, Values} ->
            Reply;
        {ok, Reply, NValues} ->
            transaction_after_update(TableKeyDefs, NValues),
            Reply;
        Err ->
            Err
    end,
    unlock(LockKeys),
    Reply1.

%% ----------------------------------------------------
%% @doc  
%%      获得redis存储的key
%% @end
%% ----------------------------------------------------
redis_str_key(TableKeys) when is_list(TableKeys) ->
    [redis_str_key(Table, Key) || {Table, Key} <- TableKeys].
redis_str_key(Table, Key) ->
    "str:" ++ term_to_string(Table) ++ ":" ++ term_to_string(Key).
%% ----------------------------------------------------
%% @doc
%%      获得redis锁key
%% @end
%% ----------------------------------------------------
redis_lock_key(TableKeys) ->
    [redis_lock_key(Table, Key) || {Table, Key} <- TableKeys].
redis_lock_key(Table, Key) ->
    "str:lock:" ++ term_to_string(Table) ++ ":" ++ term_to_string(Key).
%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc
%%      transaction批量锁
%% @end
%% ----------------------------------------------------
transaction_locks(TableKeys, TimeOut, V) ->
    MS1 = now_millisecond(),
    {Suc, Fail} = lists:foldl(fun(LockKey, {S, F}) ->
        case sync_command("SET", [LockKey, V, "EX", ?LOCKTIME, "NX"]) of
            {ok, <<"OK">>} ->
                {[LockKey | S], F};
            _ ->
                {S, [LockKey | F]}
        end
    end, {[], []}, TableKeys),
    case Fail =:= [] of
        true ->
            Suc;
        false ->
            unlock(Suc),
            MS2 = now_millisecond(),
            transaction_locks(TableKeys, TimeOut - (MS2 - MS1), V)
    end.
%% ----------------------------------------------------
%% @doc
%%      transaction修改数据
%% @end
%% ----------------------------------------------------
transaction_after_update(TableKeyDefs, NValues) ->
    sync_command("MULTL", []),
    lists:foreach(fun({{T, K, _D}, NV}) ->
        if
            NV =:= 'ignore' ->
                ok;
            true ->
                sync_command("SET", [redis_str_key(T, K), NV])
        end
    end, lists:zip(TableKeyDefs, NValues)),
    sync_command("EXEC", []).