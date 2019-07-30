%%%-------------------------------------------------------------------
%% @doc test_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(test_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-include("../include/server.hrl").

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, _} = ranch:start_listener(user_process,
        ranch_tcp, [{port, ?TCP_PORT}], user_process, []),
%%    Child1 = #{id => tcp,
%%        start => {tcp_server, start_link, []},
%%        shutdown => brutal_kill},
    {ok, {{one_for_all, 3, 10}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
