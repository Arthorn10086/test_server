-module(time_lib).

%%%=======================STATEMENT====================
-description("time_lib").
-author("yhw").

%%%=======================EXPORT=======================
-export([now_second/0, now_millisecond/0, get_zero_second/0]).

%%%=======================INCLUDE======================

%%%=======================DEFINE======================

%%%=======================RECORD=======================

%%%=======================TYPE=========================
%%-type my_type() :: atom() | integer().


%%%=================EXPORTED FUNCTIONS=================
%% ----------------------------------------------------
%% @doc  
%%        
%% @end
%% ----------------------------------------------------
now_millisecond() ->
    {M, S, MS} = os:timestamp(),
    M * 1000000000 + S * 1000 + MS div 1000.
%% ----------------------------------------------------
%% @doc
%%
%% @end
%% ----------------------------------------------------
now_second() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

get_zero_second() ->
    {TM, TS, MS} = os:timestamp(),
    Now = TM * 1000000 + TS,
    {_, {H, M, S}} = calendar:now_to_local_time({TM, TS, MS}),
    Now - (H * 60 * 60 + M * 60 + S).

%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------