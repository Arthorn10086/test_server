-module(py_test).
-behaviour(gen_server).

%% API
-export([start/0, combine/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).
-define(SERVER, {global, ?MODULE}).
-record(state, {port}).

start() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:cast(?SERVER, stop).
init([]) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, "python  e:/py/test/town.py"}, [stream, {line, 1024}]),
    {ok, #state{port = Port}}.

handle_call({combine, String}, _From, #state{port = Port} = State) ->
    port_command(Port, String),
    receive
        {Port, {data, {_Flag, Data}}} ->
            io:format("receiving:~p~n", [Data]),
            timer:sleep(2000),
            {reply, Data, Port}
    end.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(_Reason, Port) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal ---------------------------------------------------------
combine(_String) ->
    start(),
    String = list_to_binary("combine|" ++ _String ++ "\n"),
    gen_server:call(?SERVER, {combine, String}, infinity),
    stop().