-module(test_client).

%%%=======================STATEMENT====================
-description("test_client").
-author("arthorn").

%%%=======================EXPORT=======================
-export([start_link/2, init/2]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([sync_cmd/5]).
-export([echo_rev/3, call_rec/3]).

%%%=======================INCLUDE======================
-include("../../include/login_pb.hrl").
%%%=======================DEFINE======================

%%%=======================RECORD=======================
-record(state, {socket, wait_reply = [], ms}).
%%%=======================TYPE=========================
%%-type my_type() :: atom() | integer().


%%%=================EXPORTED FUNCTIONS=================
start_link(UserName, Password) ->
    proc_lib:spawn_link(fun() -> init(UserName, Password) end).

sync_cmd(UserName, Cmd, ReqRecord, PB, RespRecord) ->
    Name = list_to_atom("test_client" ++ integer_to_list(UserName)),
    gen_server:call(Name, {sync, Cmd, ReqRecord, PB, RespRecord}, 10000).
%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
init(UserName, Password) ->
    {_, C} = gen_tcp:connect("localhost", 7621, [binary, {active, true}, {packet, 0}]),
    Serial = time_lib:now_second(),
    D = protocol_routing:encode(1001, Serial, login_pb:encode_msg({'LoginReq', UserName, Password})),
    gen_tcp:send(C, D),
    receive
        {tcp, _S, Reply} ->
            R = protocol_routing:decode_reply(Reply),
            {_, S} = lists:keyfind(status, 1, R),
            {_, Serial} = lists:keyfind(serial, 1, R),
            {_, Data} = lists:keyfind(data, 1, R),
            if
                S =:= 0 ->
                    erlang:send_after(60000, self(), 'echo'),
                    gen_server:enter_loop(?MODULE, [],
                        #state{socket = C, wait_reply = [], ms = time_lib:now_millisecond()}, 10000);
                true ->
                    {false, binary_to_list(Data)}
            end
    end.
handle_call({sync, Cmd, Args, PB, RespRecord}, From, #state{socket = C, wait_reply = L} = State) ->
    Serial = time_lib:now_second(),
    D = protocol_routing:encode(Cmd, Serial, PB:encode_msg(Args)),
    gen_tcp:send(C, D),
    NL = [{Serial, PB, RespRecord, ?MODULE, call_rec, [{from, From}]} | L],
    {noreply, State#state{wait_reply = NL}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(echo, #state{socket = C, wait_reply = L} = State) ->
    D = protocol_routing:encode(1, -1, echo_pb:encode_msg({'HeartBeatReq'})),
    gen_tcp:send(C, D),
    {noreply, State#state{wait_reply = [{-1, echo_pb, 'HeartBeatResp', ?MODULE, echo_rcv, []} | L]}};
handle_info({tcp, _S, Reply}, #state{wait_reply = L} = State) ->
    R = protocol_routing:decode_reply(Reply),
    {_, S} = lists:keyfind(status, 1, R),
    {_, Serial} = lists:keyfind(serial, 1, R),
    {_, Data} = lists:keyfind(data, 1, R),
    NState = case lists:keyfind(Serial, 1, L) of
        false ->
            State;
        {_, Pb, Record, CakkBackM, CallBackF, A} ->
            NL = lists:keydelete(Serial, 1, L),
            if
                S =:= 0 ->
                    StateT = CakkBackM:CallBackF(State, A, Pb:decode_msg(Data, Record)),
                    StateT#state{wait_reply = NL};
                true ->
                    io:format("Error:~p~n", [binary_to_list(Data)]),
                    State#state{wait_reply = NL}
            end
    end,
    {noreply, NState};
handle_info(_Request, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
echo_rev(State, _A, RespRecord) ->
    State#state{ms = element(2, RespRecord)}.
call_rec(State, [{from, From}], RespRecord) ->
    gen_server:reply(From, RespRecord),
    State.
