-module(server_db).
-author("yhw").

-behaviour(supervisor).

%% API
-export([start_link/1, set/2, delete/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 5000).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%  write_through  缓存在数据变化时自动更新数据库
%%  write_behind   缓存定期批量的去更新数据库
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Type :: write_through|write_behind) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Tactics) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Tactics]).

set(Name, Options) ->
    {_, Mod} = lists:keyfind(mod, 1, Options),
    ID = server_db_lib:get_db_name(Name),
    Tactics = get_cache_tactics(),
    ChildSpec = {ID, {Mod, start_link, [ID, Name, [{'cache_tactics', Tactics} | Options]]},
        permanent, 5000, worker, [Mod]},
    supervisor:start_child(?MODULE, ChildSpec),
    ets:insert(?MODULE, {Name, Options}).
delete(Name) ->
%%    ets:delete(?MODULE, Name),
%%    ID = server_db_lib:get_db_name(Name),
%%    supervisor:terminate_child(?MODULE, ID),
%%    supervisor:delete_child(?MODULE, ID),
    ets:delete(?MODULE, Name).
get_cache_tactics() ->
    [Tactics] = ets:lookup(?MODULE, '$db_cache_tactics'),
    Tactics.
%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([Tactics]) ->
    ets:new(?MODULE, [named_table, public, set]),
    ets:insert(?MODULE, {'$db_cache_tactics', Tactics}),
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, []}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
