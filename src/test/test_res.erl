-module(test_res).

%%%=======================STATEMENT====================
-description("test_res").
-copyright('youkia,www.youkia.net').
-author("yhw,yanghaiwei@youkia.net").
-vsn(2.8).

%%%=======================EXPORT=======================
-compile(export_all).

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
t() ->
    Pid = spawn(fun() ->
        loop() end),
    register(t, Pid).


%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
loop() ->
    receive
        A ->
            io:format("=~p~n", [{?MODULE, ?LINE, A}])
    end,
    loop().


start() ->
    Pid = spawn(fun() -> do_loop() end),
    register(t, Pid).

do_loop() ->
    receive
        Msg ->
            io:format("~p~n", [Msg])
    end,
    do_loop().