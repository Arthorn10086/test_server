-module(user_process).
-author("yhw").

-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4, send/2, add_attr/2]).
-export([set_attr/2, set_echo/2]).


%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 5000).

-define(ECHO, 120000).

-define(INTERVAL, 1000).

-record(state, {socket, transport, req_queue, attr = [], current, echo, timeout_ref}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Ref, Socket, Transport, Opts) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

send(#state{socket = Socket, transport = Transport}, Data) ->
    Transport:send(Socket, Data).
add_attr(_, []) ->
    ok;
add_attr(Parent, AddList) ->
    Parent ! {add_attr, AddList}.

set_attr(State, Attr) ->
    State#state{attr = Attr}.
set_echo(State, Echo) ->
    State#state{echo = Echo}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({Ref, Socket, Transport, _Opts = []}) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
        #state{socket = Socket, transport = Transport, req_queue = queue:new(), current = none}, ?TIMEOUT).


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


%%收到消息
handle_info({tcp, Socket, Bin}, #state{socket = Socket, transport = Transport, attr = Attr, timeout_ref = Ref} = State) ->
    del_timer(Ref),
    Transport:setopts(Socket, [{active, once}]),
    Req = protocol_routing:route(Bin),
    if
        element(6, Req) == 0 ->
            ModifyAttr = protocol_routing:try_run(State, Attr, Req),
            NAttr = modify_attr(ModifyAttr, Attr),
            {noreply, State#state{attr = NAttr, timeout_ref = none}, ?TIMEOUT};
        true ->
            NState = queue_run(State, Req),
            {noreply, NState#state{timeout_ref = none}, ?TIMEOUT}
    end;

%%有进程结束
handle_info({'DOWN', _Ref, 'process', Pid, _Reason}, #state{req_queue = Queue, current = {Pid, Ref, _Req}, timeout_ref = TRef} = State) ->
    del_timer(Ref),
    del_timer(TRef),
    case queue:out(Queue) of
        {empty, _Q} ->
            {noreply, State#state{current = none, timeout_ref = none}, ?TIMEOUT};
        {{value, Req}, Q} ->
            NState = queue_run(State#state{current = none}, Req),
            {noreply, NState#state{req_queue = Q}}
    end;

%%socket断开
handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};

%%attr变化
handle_info({add_attr, L}, #state{attr = Attr} = State) ->
    {noreply, State#state{attr = modify_attr(L, Attr)}};

%%进程空闲，发送一个延时信息然后进入休眠状态
handle_info(timeout, State) ->
    {noreply, State#state{timeout_ref = erlang:start_timer(?ECHO, self(), 'echo_timeout')}, hibernate};

%%指定时间没有收到心跳,结束
handle_info({timeout, Ref, 'echo_timeout'}, #state{timeout_ref = Ref} = State) ->
    {stop, death, State};

%%当前请求执行超时
handle_info({timeout, Ref, 'current_timout'}, #state{req_queue = Queue, current = {Pid, Ref, Req}, timeout_ref = TRef} = State) ->
    del_timer(TRef),
    erlang:exit(Pid, kill),
    case queue:out(Queue) of
        {empty, _Q} ->
            {noreply, State#state{current = none, timeout_ref = none}, ?TIMEOUT};
        {{value, Req}, Q} ->
            NState = queue_run(State#state{current = none}, Req),
            {noreply, NState#state{req_queue = Q}}
    end;

handle_info(_Info, #state{socket = _Socket, transport = _Transport} = State) ->
    lager:log(info, self(), "unkown:~p~n", [_Info]),
    {noreply, State}.


terminate(death, #state{socket = Socket, transport = Tran}) ->
    Tran:close(Socket);
terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

del_timer(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref);
del_timer(_) ->
    ok.


modify_attr([], Attr) ->
    Attr;
modify_attr([{K, V} | T], Attr) ->
    modify_attr(T, maps:put(K, V, Attr));
modify_attr([_ | T], Attr) ->
    modify_attr(T, Attr).


%%空闲,直接执行
queue_run(#state{current = none, attr = Attr} = State, Req) ->
    Parent = self(),
    {Pid, _Ref} = spawn_monitor(fun() ->
        AddAttr = protocol_routing:try_run(State, Attr, Req),
        user_process:add_attr(Parent, AddAttr)
    end),
    Ref = erlang:start_timer(element(6, Req), self(), 'current_timout'),
    State#state{current = {Pid, Ref, Req}};
%%放入队列等待
queue_run(#state{req_queue = Queue} = State, Req) ->
    State#state{req_queue = queue:in(Req, Queue)}.



