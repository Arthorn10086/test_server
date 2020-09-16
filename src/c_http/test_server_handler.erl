-module(test_server_handler).

%%%=======================STATEMENT====================
-description("test_server_handler").
-author("arthorn").
-behaviour(cowboy_http_handler).
%%%=======================EXPORT=======================
-export([init/2, format/2]).

%%%=======================INCLUDE======================

%%%=======================DEFINE======================

%%%=======================RECORD=======================
-record(state, {}).
%%%=======================TYPE=========================
%%-type my_type() :: atom() | integer().


%%%=================EXPORTED FUNCTIONS=================
%% ----------------------------------------------------
%% @doc  
%%        
%% @end
%% ----------------------------------------------------
%%-spec my_function(Args1::integer()) -> integer().


%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Path0 = binary_to_list(cowboy_req:path(Req0)),
    QSKVList = cowboy_req:parse_qs(Req0),
    case config_lib:get('http_protocol', Path0) of
        none ->
            cowboy_req:reply(404, Req0);
        {_Ref, MFAList, _LogFun, {PM, PF, PA}, TimeOut} ->
            P = self(),
            spawn_link(fun() -> apply_route(Req0, P, MFAList, parse_qs1(QSKVList)) end),
            receive
                {'route_reply', Reply} ->
                    Reply1 = PM:PF(PA, Reply),
                    cowboy_req:reply(200, Req0#{resp_body => Reply1})
            after TimeOut ->
                cowboy_req:reply(500, Req0)
            end
    end,
    {ok, Req0, Opts}.
apply_route(_Req0, Parent, [], _Msg) ->
    Reply = erlang:get('$Reply'),
    Parent ! {'route_reply', Reply};
apply_route(Req0, Parent, [{M, F, A} | T], Msg) ->
    case M:F(A, Req0, Msg) of
        {ok, Req1, Reply} ->
            erlang:put('$Reply', Reply),
            apply_route(Req1, Parent, T, Msg);
        {break, Req1, Reply} ->
            Parent ! {'route_reply', Reply}
    end.

parse_qs1(QSKVL) ->
    [{binary_to_list(K), binary_to_list(V)} || {K, V} <- QSKVL].
format(A, Msg) ->
    jsx:encode(Msg).




