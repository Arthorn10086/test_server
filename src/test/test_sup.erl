-module(test_sup).


-behaviour(supervisor).

%% API
-export([start_link/0, login/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


login(UserName, PW) ->
    supervisor:start_child(?MODULE, {'test_client', {'test_client', start_link, [UserName, PW]}, permanent, 2000, worker, ['test_client']}).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    {ok, {{simple_one_for_one, 1000, 3600},
        [{test_gs, {test_gs, start_link, []}, temporary, 5000, worker, [test_gs]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
