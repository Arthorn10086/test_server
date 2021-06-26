-module(protocol_routing).

%%%=======================STATEMENT====================
-description("protocol_routing").
-author("arthorn").


%%%=======================EXPORT=======================
-export([encode/3, decode/1, encode_reply/4, decode_reply/1, route/1, try_run/3]).

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
%%encode(Protocol, Serial, Data) ->
%%    jsx:encode([{<<"cmd">>, Protocol}, {<<"serial">>, Serial}, {<<"data">>, Data}]).
%%decode(Bin) ->
%%    jsx:decode(Bin, [{labels, atom}]).
%%encode_reply(error, Serial, Data, _PbMod) ->
%%    jsx:encode([{<<"status">>, 1}, {<<"serial">>, Serial}, {<<"data">>, Data}]);
%%encode_reply(ok, Serial, Data, PbMod) ->
%%    jsx:encode([{<<"status">>, 0}, {<<"serial">>, Serial}, {<<"data">>, PbMod:encode_msg(Data)}]).

encode(Protocol, Serial, Msg) ->
    B1 = term_to_binary(Protocol),
    BZ1 = byte_size(B1),
    BZ2 = byte_size(Msg),
    Data = <<Serial:32, BZ1:16/integer, B1/binary, BZ2:16, Msg/binary>>,
    Size = byte_size(Data),
    <<Size:16, Data/binary>>.

decode(<<Size:16, Data:Size/binary, _T/binary>>) ->
    <<Serial:32, BZ1:16, Protocol:BZ1/binary, BZ2:16, Msg:BZ2/binary>> = Data,
    [{'protocol', binary_to_term(Protocol)}, {'serial', Serial}, {'data', Msg}].

%%|<-(8位 状态码)->|<-(32位 流水号)->|<- Reply ->|
encode_reply(Status, Serial, Reply, PbMod) ->
    {B1, B3} = case Status of
        'error' ->
            {1, term_to_binary(Reply)};
        'ok' ->
            {0, PbMod:encode_msg(Reply)}
    end,
    BZ3 = byte_size(B3),
    Data = <<Serial:32/integer, B1:8/integer, BZ3:16, B3/binary>>,
    Size = byte_size(Data),
    <<Size:16, Data/binary>>.

decode_reply(<<Size:16, Data:Size/binary, _T/binary>>) ->
    <<Serial:32, Status:8, BZ2:16, Reply:BZ2/binary>> = Data,
    [{'serial', Serial}, {'status', Status}, {'data', Reply}].

%% ----------------------------------------------------
%% @doc  
%%      路由
%% @end
%% ----------------------------------------------------
route(Bin) ->
    try
        R = decode(Bin),
        {_, Cmd} = lists:keyfind('protocol', 1, R),
        {_, Serial} = lists:keyfind('serial', 1, R),
        {_, Data} = lists:keyfind('data', 1, R),
        {_, MFAList, ErrorLog, {PbMod, Req, _Resp}, Timeout} = config_lib:get('tcp_protocol', Cmd),
        Data1 = PbMod:decode_msg(Data, Req),
        statistics_protocol(Cmd),
        {Data1, Serial, MFAList, ErrorLog, PbMod, Timeout}
    catch
        Class:Reason ->
            lager:log(error, self(), "~s", [lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})])
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



try_run(Session, Attr, {Data, Serial, MFAList, {LogM, LogF, _}, PbMod, _}) ->
    try
        {Status, ModifyAttr, Reply} = handle_mfa(MFAList, Session, Attr, Data),
        RData = encode_reply(Status, Serial, Reply, PbMod),
        user_process:send(Session, RData),
        ModifyAttr
    catch
        E1:E2 ->
            LogM:LogF(self(), lager:pr_stacktrace(erlang:get_stacktrace(), {E1, E2})),
            Error = encode_reply(error, Serial, erlang:get_stacktrace(), PbMod),
            user_process:send(Session, Error)
    end.


statistics_protocol(Cmd) ->
    case whereis('statistics_tool') of
        P when is_pid(P) ->
            P ! {'protocol', Cmd};
        _ ->
            ok
    end.