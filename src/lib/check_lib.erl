-module(check_lib).

%%%=======================STATEMENT====================
-description("check_lib").
-author("yhw").

%%%=======================EXPORT=======================
-export([check_all/4, get_bool_value/3]).

%%%=======================INCLUDE======================

%%%=======================DEFINE======================

%%%=======================RECORD=======================

%%%=======================TYPE=========================
%%-type my_type() :: atom() | integer().


%%%=================EXPORTED FUNCTIONS=================
check_all([], _, _, _) ->
    true;
check_all([Conditon | T], MF, Args, Input) ->
    Result = case MF of
        {M, F} ->
            M:F(Conditon, Args, Input);
        Fun when is_function(Fun, 3) ->
            Fun(Conditon, Args, Input)
    end,
    if
        Result ->
            check_all(T, MF, Args, Input);
        true ->
            Result
    end.



get_bool_value(true, R1,_R2) ->
    R1;
get_bool_value(false, _R1,R2) ->
    R2.

%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
