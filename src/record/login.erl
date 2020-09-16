-module(login).
%%%=======================STATEMENT====================
-description("login").
-copyright(arthorn10086).
-author("arthorn10086").
%%%=======================EXPORT====================
-export([init_LoginReq/0, get_LoginReq_fields/0]).
-export([get_user/1, get_password/1]).
-export([set_user/2, set_password/2]).
-export([init_loginResp_RoleInfo/0, get_loginResp_RoleInfo_fields/0]).
-export([get_role_uid/1, get_role_name/1, get_role_lv/1, get_profession/1, get_figure/1]).
-export([set_role_uid/2, set_role_name/2, set_role_lv/2, set_profession/2, set_figure/2]).
-export([init_loginResp/0, get_loginResp_fields/0]).
-export([get_now_ms/1, get_role_info/1]).
-export([set_now_ms/2, set_role_info/2]).
-export_type(['LoginReq'/0, 'loginResp.RoleInfo'/0,      loginResp/0]).
%%%=======================RECORD====================
-record('LoginReq',{
	user  :: non_neg_integer() | undefined,
	password  :: iodata() | undefined}).
-record('loginResp.RoleInfo',{
	role_uid  :: non_neg_integer() | undefined,
	role_name  :: iodata() | undefined,
	role_lv  :: non_neg_integer() | undefined,
	profession  :: non_neg_integer() | undefined,
	figure  :: non_neg_integer() | undefined}).
-record(loginResp,{
	now_ms  :: non_neg_integer() | undefined,
	role_info = []  ::     [login_pb:'loginResp.RoleInfo'()] | undefined}).
%%%=======================TYPE====================
-type 'LoginReq'() :: #'LoginReq'{}.
-type 'loginResp.RoleInfo'() :: #'loginResp.RoleInfo'{}.
-type loginResp() :: #loginResp{}.
	
%%%=======================EXPORTED FUNCTIONS====================
	
%% ----------------------------------------------------	
%% @doc	
%%	loginRespinit函数	
%% @end	
%% ----------------------------------------------------	
init_loginResp()-> #'loginResp'{}.
get_loginResp_fields()-> record_info(fields, 'loginResp').
	
%% ----------------------------------------------------	
%% @doc	
%%	loginRespget函数	
%% @end	
%% ----------------------------------------------------	
get_now_ms(#loginResp{now_ms = V})->V.	
get_role_info(#loginResp{role_info = V})->V.	
	
%% ----------------------------------------------------	
%% @doc	
%%	loginRespset函数	
%% @end	
%% ----------------------------------------------------	
set_now_ms(Record,V)->
	Record#loginResp{now_ms = V}.	
set_role_info(Record,V)->
	Record#loginResp{role_info = V}.	
	
%% ----------------------------------------------------	
%% @doc	
%%	loginResp_RoleInfoinit函数	
%% @end	
%% ----------------------------------------------------	
init_loginResp_RoleInfo()-> #'loginResp.RoleInfo'{}.
get_loginResp_RoleInfo_fields()-> record_info(fields, 'loginResp.RoleInfo').
	
%% ----------------------------------------------------	
%% @doc	
%%	loginResp_RoleInfoget函数	
%% @end	
%% ----------------------------------------------------	
get_role_uid(#'loginResp.RoleInfo'{role_uid = V})->V.	
get_role_name(#'loginResp.RoleInfo'{role_name = V})->V.	
get_role_lv(#'loginResp.RoleInfo'{role_lv = V})->V.	
get_profession(#'loginResp.RoleInfo'{profession = V})->V.	
get_figure(#'loginResp.RoleInfo'{figure = V})->V.	
	
%% ----------------------------------------------------	
%% @doc	
%%	loginResp_RoleInfoset函数	
%% @end	
%% ----------------------------------------------------	
set_role_uid(Record,V)->
	Record#'loginResp.RoleInfo'{role_uid = V}.	
set_role_name(Record,V)->
	Record#'loginResp.RoleInfo'{role_name = V}.	
set_role_lv(Record,V)->
	Record#'loginResp.RoleInfo'{role_lv = V}.	
set_profession(Record,V)->
	Record#'loginResp.RoleInfo'{profession = V}.	
set_figure(Record,V)->
	Record#'loginResp.RoleInfo'{figure = V}.	
	
%% ----------------------------------------------------	
%% @doc	
%%	LoginReqinit函数	
%% @end	
%% ----------------------------------------------------	
init_LoginReq()-> #'LoginReq'{}.
get_LoginReq_fields()-> record_info(fields, 'LoginReq').
	
%% ----------------------------------------------------	
%% @doc	
%%	LoginReqget函数	
%% @end	
%% ----------------------------------------------------	
get_user(#'LoginReq'{user = V})->V.	
get_password(#'LoginReq'{password = V})->V.	
	
%% ----------------------------------------------------	
%% @doc	
%%	LoginReqset函数	
%% @end	
%% ----------------------------------------------------	
set_user(Record,V)->
	Record#'LoginReq'{user = V}.	
set_password(Record,V)->
	Record#'LoginReq'{password = V}.	
