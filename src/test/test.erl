-module(test).

%%%=======================STATEMENT====================
-description("test").
-author("yhw").

%%%=======================EXPORT=======================
-export([test_update/4, test_client/0, test_send/4]).

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
test_update(Tab, Key, Key1, Value) ->
    F = fun(V, _Args) ->
        NV = maps:put(Key1, Value, V),
        {ok, {V, NV}, NV}
    end,
    server_db_client:update(Tab, Key, F, [], []).


%%test_transaction() ->
%%    F1 = fun(_Args, [{Index1, V}, {Index2, V2}]) ->
%%        NV = maps:put(password, "1", V),
%%        NV2 = maps:put(password, "2", V2),
%%        {ok, ok, [{Index1, NV}, {Index2, NV2}]}
%%    end,
%%    server_db_client:transaction([{user, 1, 1}, {user, 2, 2}], F1, []).



test_client() ->
    {_, C} = gen_tcp:connect("localhost", 7620, [binary, {active, true}, {packet, 0}]),
    gen_tcp:controlling_process(C, self()),
    C.
test_send(C, P, S, D) ->
    JS = protocol_routing:encode(P, S, D),
    gen_tcp:send(C, JS).
%%    receive
%%        Info ->
%%            io:format("~p~n", [Info])
%%    end.


%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
