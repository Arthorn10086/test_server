-module(user_process).
-author("yhw").

-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4, send/2, add_attr/2]).
-export([set_attr/2, set_echo/2]).
-export([add_run/6]).

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

-record(state, {socket, transport, attr = [], run = [], echo}).

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
    gen_server:call(Parent, {add_attr, AddList}).

set_attr(State, Attr) ->
    State#state{attr = Attr}.
set_echo(State, Echo) ->
    State#state{echo = Echo}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).

init({Ref, Socket, Transport, _Opts = []}) ->
    ok = ranch:accept_ack(Ref),
    register(test_user, self()),
    ok = Transport:setopts(Socket, [{active, once}]),
    erlang:send_after(?INTERVAL, self(), 'echo'),
    gen_server:enter_loop(?MODULE, [],
        #state{socket = Socket, transport = Transport},
        ?TIMEOUT).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({add_attr, AddList}, _From, #state{attr = Attr} = State) ->
    {reply, ok, State#state{attr = AddList ++ Attr}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({tcp, Socket, Bin}, #state{socket = Socket, transport = Transport, attr = Attr} = State) ->
    Transport:setopts(Socket, [{active, once}]),
    MS = time_lib:now_millisecond(),
    NState = protocol_routing:route(State, Attr, Bin, MS),
    {noreply, NState#state{echo = MS}};
handle_info({'DOWN', Ref, 'process', _Pid, _Reason}, #state{run = Run} = State) ->
    Run1 = lists:keydelete(Ref, 3, Run),
    {noreply, State#state{run = Run1}};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_info(echo, #state{run = Run, echo = LastTime} = State) ->
    MS = time_lib:now_millisecond(),
    NRun = handle_time_out(Run, MS),
    if
        MS - LastTime > ?ECHO ->
            {stop, death, State};
        true ->
            {noreply, State#state{run = NRun}}
    end;
handle_info(_Info, #state{socket = Socket, transport = Transport} = State) ->
    io:format("unkown:~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: #state{}) -> term()).
terminate(death, _State) ->
    lager:info("echo_time_out");
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
        Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_run(#state{run = Run} = State, Pid, Ref, Cmd, MS, EndTime) ->
    NRun = lists:keysort(1, [{MS + EndTime, Pid, Ref, Cmd} | Run]),
    State#state{run = NRun, echo = MS}.

handle_time_out([], _MS) ->
    [];
handle_time_out([{EndTime, _, _, _} | _T] = L, MS) when EndTime > MS ->
    L;
handle_time_out([{EndTime, Pid, Ref, Cmd} | T], MS) ->
    erlang:exit(Pid, 'timeout'),
    log_lib:error(Pid, [{EndTime, Ref, Cmd}]),
    handle_time_out(T, MS).
