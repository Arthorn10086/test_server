-module(login_port).

%%%=======================STATEMENT====================
-description("login_port").
-author("yhw").

%%%=======================EXPORT=======================
-export([login/4, check/3]).

%%%=======================INCLUDE======================

%%%=======================DEFINE======================
-include("../include/login_pb.hrl").
%%%=======================RECORD=======================

%%%=======================TYPE=========================
%%-type my_type() :: atom() | integer().


%%%=================EXPORTED FUNCTIONS=================
%% ----------------------------------------------------
%% @doc  
%%        
%% @end
%% ----------------------------------------------------
login(_A, _Session, _Attr, Msg) ->
    #'LoginReq'{user = UserName, password = Password} = Msg,
    {ok, Maps, _, _} = server_db_client:get('user', UserName, none),
    case check_lib:check_all(['exist', {'password', Password}], {?MODULE, check}, 'login', Maps) of
        true ->
            {ok, [], #loginResp{now_ms = 111, role_info = []}};
        Err ->
            {error, [], Err}
    end.


%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
check('exist', 'login', Maps) ->
    check_lib:get_bool_value(Maps =/= none, true, "no_exist_user");
check({'password', PW}, 'login', Maps) ->
    check_lib:get_bool_value(maps:get('password', Maps) =:= PW, true, "password_error");
check(_, _, _) ->
    "undefined_condition".