-module(test).

%%%=======================STATEMENT====================
-description("test").
-author("arthorn").

%%%=======================EXPORT=======================
-export([test_update/4, test_client/0, test_send/4, add/3, try_apply/3]).
-export([rqps/2]).

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



rqps(QNum, PNum) ->
    eredis_lib:sync_command("CONFIG",["RESETSTAT"]),
    PQNum = QNum div PNum,
    PQList = lists:foldl(fun(PIndex, R) ->
        SIndex = PQNum * (PIndex - 1) + 1,
        EIndex = PQNum * PIndex,
        [lists:seq(SIndex, EIndex) | R]
    end, [], lists:seq(1, PNum)),
    From = self(),
    erlang:send_after(100, self(), time_out1),
    PidL = lists:foldl(fun(QL, R) ->
        Pid = spawn(fun() ->
            lists:foreach(fun(I) ->
                eredis_lib:sync_command("GET", [I])
            end, QL),
            timer:sleep(200),
            From ! {self(), over}
        end),
        [Pid | R]
    end, [], PQList),
    loop("./rqps.txt", PidL),
    ok.



%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
loop(F, PidL) ->
    receive
        time_out1 ->
            erlang:send_after(100, self(), time_out1),
            spawn(fun() ->
                B = element(2, eredis_lib:sync_command("INFO", [stats])),
                %%lists:nth(3, binary:split(B, <<"\r\n">>, [global]))
                file:write_file(F, B, [append]),
                file:write_file(F, <<"\r\n">>, [append])
            end),
            loop(F, PidL);
        {Pid, over} ->
            case lists:delete(Pid, PidL) of
                [] ->
                    receive
                        _ ->
                            ok
                    end;
                L1 ->
                    loop(F, L1)
            end
    end.
