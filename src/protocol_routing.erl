-module(protocol_routing).

%%%=======================STATEMENT====================
-description("protocol_routing").
-author("yhw").


%%%=======================EXPORT=======================
-export([encode/3, decode/1, encode_reply/2, route/3]).

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
    jsx:decode(Bin).
encode_reply(Status, Data) ->
    jsx:encode([{<<"status">>, Status}, {<<"data">>, Data}]).
%% ----------------------------------------------------
%% @doc  
%%      路由
%% @end
%% ----------------------------------------------------
route(Socket, Transport, Bin) ->
    try
        Json = decode(Bin),
        {_, Cmd} = lists:keyfind(<<"cmd">>, 1, Json),
        {_, MFAList, {LogM, LogF, LogA}, Timeout} = config_lib:get('tcp_protocol', Cmd),
        {Pid, Ref} = spawn_monitor(fun() ->
            try
                %%TODO
                handle_mfa(MFAList, Transport, Socket)
            catch
                E1:E2 ->
                    LogM:LogF(LogA, E1, E2, erlang:get_stacktrace())
            end
        end),
        {Pid, Ref, Timeout}
    catch
        _:_ ->
            encode_reply(error, erlang:get_stacktrace())
    end.


%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
