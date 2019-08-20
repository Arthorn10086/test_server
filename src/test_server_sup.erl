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
    {ok, Port} = application:get_env(port),
    {ok, Path} = application:get_env(cfg_path),
    {ok, _} = ranch:start_listener(user_process,
        ranch_tcp, [{port, Port}], user_process, []),
    Child1 = #{id => config_lib,
        start => {config_lib, start_link, []},
        shutdown => brutal_kill},
    Child2 = #{id => server_event,
        start => {server_event, start_link, []},
        shutdown => brutal_kill},
    Child3 = #{id => server_timer,
        start => {server_timer, start_link, []},
        shutdown => brutal_kill},
    Child4 = #{id => server_db,
        start => {server_db, start_link, []},
        shutdown => brutal_kill,
        type => supervisor},
    Child5 = #{id => file_monitor,
        start => {file_monitor, start_link, [Path]},
        shutdown => brutal_kill},


    {ok, {{one_for_all, 3, 10}, [Child1, Child2, Child3, Child4, Child5]}}.

%%====================================================================
%% Internal functions
%%====================================================================
