-module(test).

%%%=======================STATEMENT====================
-description("test").
-author("arthorn").

%%%=======================EXPORT=======================
-export([test_update/4, test_client/0, test_send/4, add/3, try_apply/3]).

%%%=======================INCLUDE======================

%%%=======================DEFINE======================

%%%=======================RECORD=======================

%%%=======================TYPE=========================
%%-type my_type() :: atom() | integer().


%%%=================EXPORTED FUNCTIONS=================

try_apply(M, F, A) ->
    try
        apply(M, F, A)
    catch
        E1 :E2 ->
            io:format("~p~n", [{E1, E2, erlang:get_stacktrace()}])
    end.


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

test_lock() ->
    fun() ->
        spawn(fun() ->
            server_db_client:update(user, 11, fun(V, _Args) ->
                timer:sleep(3500),
                NV = maps:put(password, "test", V),
                {ok, NV, NV}
            end, [], [])
        end),
        timer:sleep(100),
        spawn(fun() ->
            server_db_client:update(user, 11, fun(V, _) ->
                NV = maps:put(password, "test11", V),
                {ok, NV, NV}
            end, [], [])

        end),
        io:format("~p~n", [server_db_client:get(user, 11, [])]),
        timer:sleep(1000),
        io:format("~p~n", [server_db_client:get(user, 11, [])]),
        timer:sleep(1000),
        io:format("~p~n", [server_db_client:get(user, 11, [])]),
        timer:sleep(1000),
        io:format("~p~n", [server_db_client:get(user, 11, [])]),
        timer:sleep(1000),
        io:format("~p~n", [server_db_client:get(user, 11, [])]),
        timer:sleep(1000),
        io:format("~p~n", [server_db_client:get(user, 11, [])]),
        timer:sleep(1000),
        io:format("~p~n", [server_db_client:get(user, 11, [])])
    end,
    ok.


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


add(_, Q, Msg) ->
    A = list_to_integer(list_lib:get_value("a", 1, Msg, "0")),
    B = list_to_integer(list_lib:get_value("b", 1, Msg, "0")),
    {ok, Q, [{reply, A + B}]}.

%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
