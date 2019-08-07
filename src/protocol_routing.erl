-module(protocol_routing).

%%%=======================STATEMENT====================
-description("protocol_routing").
-author("yhw").


%%%=======================EXPORT=======================
-export([encode/3, decode/1, encode_reply/3, route/4]).

%%%=======================INCLUDE======================

%%%=======================DEFINE======================

%%%=======================RECORD=======================

%%%=======================TYPE=========================
%%-type my_type() :: atom() | integer().


%%%=================EXPORTED FUNCTIONS=================
%% ----------------------------------------------------
%% @doc
%%      组包解包
%% @end
%% ----------------------------------------------------
encode(Protocol, Serial, Data) ->
    jsx:encode([{<<"cmd">>, Protocol}, {<<"serial">>, Serial}, {<<"data">>, Data}]).
decode(Bin) ->
    [{binary:bin_to_list(K), V} || {K, V} <- jsx:decode(Bin)].
encode_reply(Status, Serial, Data) ->
    jsx:encode([{<<"status">>, Status}, {<<"serial">>, Serial}, {<<"data">>, Data}]).


%% ----------------------------------------------------
%% @doc  
%%      路由
%% @end
%% ----------------------------------------------------
route(Session, Attr, Bin, MS) ->
    Parent = self(),
    Msg = decode(Bin),
    {_, Cmd} = lists:keyfind("cmd", 1, Msg),
    {_, Serial} = lists:keyfind("serial", 1, Msg),
    {_, MFAList, {LogM, LogF, LogA}, Timeout} = config_lib:get('tcp_protocol', Cmd),
    {Pid, Ref} = spawn_monitor(fun() ->
        try
            {Status, AddAttr, Reply} = handle_mfa(MFAList, Session, Attr, Msg),
            Data = encode_reply(Status, Serial, Reply),
            user_process:add_attr(Parent, AddAttr),
            user_process:send(Session, Data)
        catch
            E1:E2 ->
                LogM:LogF(LogA ++ [{E1, E2, xmerl_ucs:to_utf8(erlang:get_stacktrace())}]),
                Error = encode_reply(error, Serial, erlang:get_stacktrace()),
                user_process:send(Session, Error)
        end
    end),
    user_process:add_run(Session, Pid, Ref, Cmd, MS , Timeout).


%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
handle_mfa(MFAL, Session, Attr, Msg) ->
    handle_mfa(MFAL, Session, Attr, Msg, []).
handle_mfa([], _Session, _Attr, Msg, AddAttr) ->
    {ok, AddAttr, Msg};
handle_mfa([{M, F, A} | T], Session, Attr, Msg, AddAttr) ->
    case M:F(A, Session, Attr, Msg) of
        {break, AddList, Reply} ->
            {ok, AddList, Reply};
        {ok, AddList, NMsg} ->
            handle_mfa(T, Session, AddList ++ Attr, NMsg, AddList ++ AddAttr)
    end.