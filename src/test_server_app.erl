%%%-------------------------------------------------------------------
%% @doc test_server public API
%% @end
%%%-------------------------------------------------------------------

-module(test_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(tcp_port),
    {ok, _} = ranch:start_listener(user_process,
        ranch_tcp, [{port, Port}], user_process, []),
    {ok, HttpPort} = application:get_env(http_port),
    Routes = [
        {'_', [
            {"/[...]", test_server_handler, []}
        ]}
    ],

    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(http, [{port, HttpPort}], #{env =>#{dispatch => Dispatch}}),
    test_server_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
