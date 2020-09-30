-module(eredis_lib).

%%%=======================STATEMENT====================
-description("eredis_lib").
-author("arthorn").
-vsn(1.0).

-import(db_lib, [term_to_string/1, string_to_term/1, now_millisecond/0, now_second/0]).
%%%=======================EXPORT=======================
-export([sync_command/2, sync_command/3, sync_command/4, async_command/2, async_command/3]).
-export([lock/2, lock/4, unlock/1]).
-export([get/2, gets/1]).
-export([dirty_write/3, dirty_write/4, dirty_write/5]).
-export([update/5]).

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
get(Table, Key) ->
    sync_command("GET", [redis_str_key(Table, Key)]).

gets(TableKeys) ->
    sync_command("MGET", [redis_str_key(TableKeys)]).

dirty_write(Table, Key, Value) ->
    sync_command("SET", [redis_str_key(Table, Key), Value]).
dirty_write(Table, Key, Value, EX) when is_integer(EX) ->
    sync_command("SET", [redis_str_key(Table, Key), Value, "EX", EX]);
dirty_write(Table, Key, Value, NX) when is_boolean(NX) ->
    if
        NX ->
            sync_command("SET", [redis_str_key(Table, Key), Value, "NX"]);
        true ->
            dirty_write(Table, Key, Value)
    end.
dirty_write(Table, Key, Value, NX, EX) ->
    if
        NX ->
            sync_command("SET", [redis_str_key(Table, Key), Value, "EX", EX, "NX"]);
        true ->
            dirty_write(Table, Key, Value, EX)
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
    V = term_to_string(self()),
    lock_(LockKey, V, LockTime, TimeOut).

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
unlock(TableKeys) ->
    LockKeys = redis_lock_key(TableKeys),
    V = list_to_binary(term_to_string(self())),
    sync_command("WATCH", LockKeys),
    {ok, VL} = sync_command("MGET", LockKeys),
    sync_command("MULTL", []),
    lists:foreach(fun({LockKey, V1}) ->
        case V =:= V1 of
            true ->
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
update(Table, Key, Fun, Args, TimeOut) ->
    lock(Table, Key, ?LOCKTIME, TimeOut),
    OldV = get(Table, Key),
    try
        case Fun(Args, OldV) of
            {ok, Reply} ->
                Reply;
            {ok, Reply, OldV} ->
                Reply;
            {ok, Reply, NV} ->
                dirty_write(Table, Key, NV),
                Reply;
            Other ->
                throw({?MODULE, ?FUNCTION_NAME, 'bad_return', Other})
        end
    catch
        _EType:Reason ->
            Reason
    after
        unlock([{Table, Key}])
    end.

%%transaction(TableKeyDefs, F, Args, TimeOut) ->
%%    TableKeys = [redis_lock_key(T, K) || {T, K, _D} <- TableKeyDefs],
%%    transaction_locks(TableKeys, TimeOut, term_to_string(self())),
%%    ok.
%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%      获得redis存储的key
%% @end
%% ----------------------------------------------------
redis_str_key(TableKeys) when is_list(TableKeys) ->
    [redis_str_key(Table, Key) || {Table, Key} <- TableKeys].
redis_str_key(Table, Key) ->
    "str:" ++ term_to_string(Table) ++ ":" ++ term_to_string(Key).
redis_lock_key(TableKeys) ->
    [redis_lock_key(Table, Key) || {Table, Key} <- TableKeys].
redis_lock_key(Table, Key) ->
    "str:lock:" ++ term_to_string(Table) ++ ":" ++ term_to_string(Key).

%%transaction_locks(TableKeys, TimeOut, V) ->
%%    {Suc, Fail} = lists:foldl(fun(LockKey, {S, F}) ->
%%        case sync_command("SET", [LockKey, V, "EX", ?LOCKTIME, "NX"]) of
%%            {ok, <<"OK">>} ->
%%                {[LockKey | S], F};
%%            _ ->
%%                {S, [LockKey | F]}
%%        end
%%    end, {[], []}, TableKeys),
%%    case Fail =:= [] of
%%        true ->
%%            ok;
%%        false ->
%%            ok
%%    end.