-module(robot_manager).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/2, start_batch/2, sync_cmd/3, get_count_robot/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {ets}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


start(UserName, PW) ->
    ?MODULE:start_link(),
    case test_client:start_link(UserName, PW) of
        {false, Error} ->
            io:format("~p~n", [Error]);
        Pid ->
            ets:insert(?MODULE, {UserName, Pid})
    end.

start_batch(List, Interval) ->
    lists:foreach(fun({U, PW}) ->
        start(U, PW),
        timer:sleep(Interval)
    end, List).

sync_cmd(UserName, Cmd, ReqRecord) ->
    [{_, Pid}] = ets:lookup(?MODULE, UserName),
    {_, _, _, {PbMod, _, RespName}, _} = config_lib:get('tcp_protocol', Cmd),
    gen_server:call(Pid, {sync, Cmd, ReqRecord, PbMod, RespName}, 10000).


get_count_robot() ->
    ets:info(?MODULE, size).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    erlang:process_flag(trap_exit, true),
    Ets = ets:new(?MODULE, [public, named_table]),
    {ok, #state{ets = Ets}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    io:format("~p~n", [_Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
