-module(test_client).

%%%=======================STATEMENT====================
-description("test_client").
-author("yhw").

%%%=======================EXPORT=======================
-export([test_login/2,login/2]).

%%%=======================INCLUDE======================
-include("../../include/login_pb.hrl").
%%%=======================DEFINE======================

%%%=======================RECORD=======================
-record(state, {socket, wait_reply = [], ms}).
%%%=======================TYPE=========================
%%-type my_type() :: atom() | integer().


%%%=================EXPORTED FUNCTIONS=================
test_login(UserName, Password) ->
    proc_lib:spawn_link(fun() -> login(UserName, Password) end).


%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
login(UserName, Password) ->
    register("test_clinet" ++ integer_to_list(UserName), self()),
    {_, C} = gen_tcp:connect("localhost", 7620, [binary, {active, true}, {packet, 0}]),
    gen_tcp:controlling_process(C, self()),
    Serial = time_lib:now_millisecond(),
    D = protocol_routing:encode(1001, Serial, login_pb:encode_msg({'LoginReq', UserName, Password})),
    gen_tcp:send(C, D),
    receive
        {tcp, _S, Reply} ->
            R = protocol_routing:decode(Reply),
            S = lists:keyfind("status", 1, R),
            Serial = lists:keyfind("serial", 1, R),
            Data = lists:keyfind("data", 1, R),
            if
                S =:= 0 ->
                    login_after(login_pb:decode_msg(Data, 'loginResp')),
                    erlang:send_after(60000, 'echo', []),
                    State = #state{socket = C, wait_reply = [], ms = Serial},
                    loop(State);
                true ->
                    {false, Data}
            end
    end.

loop(#state{socket = C ,wait_reply = L} = State) ->
    receive
        {tcp, _S, Reply} ->
            R = protocol_routing:decode(Reply),
            S = lists:keyfind("status", 1, R),
            Serial = lists:keyfind("serial", 1, R),
            Data = lists:keyfind("data", 1, R),
            case lists:keyfind(Serial, 1, L) of
                false ->
                    loop(State);
                {_, Pb, Record, CakkBackM, CallBackF} ->
                    if
                        S =:= 0 ->
                            CakkBackM:CallBackF(Pb:decode_msg(Data, Record));
                        true ->
                            io:format("Error:~p~n", [Data])
                    end
            end,
            loop(State);
        echo->
            D = protocol_routing:encode(1, 0, echo_pb:encode_msg({'HeartBeatReq'})),
            gen_tcp:send(C, D),
            ok;
        E ->
            io:format("unkown_message:~p~n", [E])
    end.


login_after(R) ->
    io:format("~p~n", [R]).