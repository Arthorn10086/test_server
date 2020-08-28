-module(eredis_lib).

%%%=======================STATEMENT====================
-description("eredis_lib").
-author("yhw").
-vsn(1.0).

%%%=======================EXPORT=======================
-export([sync_command/2, sync_command/3, sync_command/4, async_command/2, async_command/3]).

%%%=======================INCLUDE======================

%%%=======================DEFINE======================
-define(POOLNAME, dbsrv).
-define(TIMEOUT, 5000).
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


%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
