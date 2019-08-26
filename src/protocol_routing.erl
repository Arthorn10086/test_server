-module(protocol_routing).

%%%=======================STATEMENT====================
-description("protocol_routing").
-author("yhw").


%%%=======================EXPORT=======================
-export([encode/3, decode/1, encode_reply/4, route/4]).

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
encode_reply(error, Serial, Data, _PbMod) ->
    jsx:encode([{<<"status">>, 1}, {<<"serial">>, Serial}, {<<"data">>, Data}]);
encode_reply(ok, Serial, Data, PbMod) ->
    jsx:encode([{<<"status">>, 0}, {<<"serial">>, Serial}, {<<"data">>, PbMod:encode_msg(Data)}]).


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
    {_, Data} = lists:keyfind("data", 1, Msg),
    {_, MFAList, {LogM, LogF, _LogA}, {PbMod, Req}, Timeout} = config_lib:get('tcp_protocol', Cmd),
    Data1 = PbMod:decode_msg(Data, Req),
    if
        Timeout =:= 0 ->%%同步访问
            {AddAttr, Reply} = try_run(MFAList, Session, Attr, Data1, Serial, PbMod,  LogM, LogF),
            Session1 = user_process:set_attr(Session, AddAttr ++ Attr),
            if
                Cmd =:= 1 ->%%心跳
                    user_process:set_echo(Session1, element(2, Reply));
                true ->
                    Session1
            end;
        true ->
            {Pid, Ref} = spawn_monitor(fun() ->
                {AddAttr, _} = try_run(MFAList, Session, Attr, Data1, Serial, PbMod,  LogM, LogF),
                user_process:add_attr(Parent, AddAttr)
            end),
            user_process:add_run(Session, Pid, Ref, Cmd, MS, Timeout)
    end.

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
        {error, L, Reply} ->
            {error, L, Reply};
        {ok, AddList, NMsg} ->
            handle_mfa(T, Session, AddList ++ Attr, NMsg, AddList ++ AddAttr)
    end.



try_run(MFAList, Session, Attr, Data1, Serial, PbMod, LogM, LogF) ->
    try
        {Status, AddAttr, Reply} = handle_mfa(MFAList, Session, Attr, Data1),
        RData = encode_reply(Status, Serial, Reply, PbMod),
        user_process:send(Session, RData),
        {AddAttr, Reply}
    catch
        E1:E2 ->
            LogM:LogF(self(), lager:pr_stacktrace(erlang:get_stacktrace(), {E1, E2})),
            Error = encode_reply(error, Serial, erlang:get_stacktrace(), PbMod),
            user_process:send(Session, Error)
    end.