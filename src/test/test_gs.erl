%%%-------------------------------------------------------------------
%%% @author yhw,yanghaiwei@youkia.net
%%% @copyright (C) 2020, youkia,www.youkia.net
%%% @doc
%%%
%%% @end
%%% Created : 02. 九月 2020 16:27
%%%-------------------------------------------------------------------
-module(test_gs).
-author("yhw,yanghaiwei@youkia.net").

-behaviour(gen_server).

%% API
-export([start_link/0]).

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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    E = ets:new(?MODULE, [named_table, public]),
    {ok, #state{ets = E}}.


handle_call({call,1}, _From, State) ->
    {reply, yes, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    ets:insert(yhw,{_OldVsn,_Extra}),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
