-module(register).
%%%=======================STATEMENT====================
-description("register").
-copyright(arthorn10086).
-author("arthorn10086").
%%%=======================EXPORT====================
-export([init_RegReq/0, get_RegReq_fields/0]).
-export([get_user/1, get_password/1]).
-export([set_user/2, set_password/2]).
-export([init_RegResp/0, get_RegResp_fields/0]).
-export([get_status/1]).
-export([set_status/2]).
-export_type(['RegReq'/0, 'RegResp'/0]).
%%%=======================RECORD====================
-record('RegReq',{
	user  :: non_neg_integer() | undefined,
	password  :: iodata() | undefined}).
-record('RegResp',{
	status  :: non_neg_integer() | undefined}).
%%%=======================TYPE====================
-type 'RegReq'() :: #'RegReq'{}.
-type 'RegResp'() :: #'RegResp'{}.
	
%%%=======================EXPORTED FUNCTIONS====================
	
%% ----------------------------------------------------	
%% @doc	
%%	RegRespinit函数	
%% @end	
%% ----------------------------------------------------	
init_RegResp()-> #'RegResp'{}.
get_RegResp_fields()-> record_info(fields, 'RegResp').
	
%% ----------------------------------------------------	
%% @doc	
%%	RegRespget函数	
%% @end	
%% ----------------------------------------------------	
get_status(#'RegResp'{status = V})->V.	
	
%% ----------------------------------------------------	
%% @doc	
%%	RegRespset函数	
%% @end	
%% ----------------------------------------------------	
set_status(Record,V)->
	Record#'RegResp'{status = V}.	
	
%% ----------------------------------------------------	
%% @doc	
%%	RegReqinit函数	
%% @end	
%% ----------------------------------------------------	
init_RegReq()-> #'RegReq'{}.
get_RegReq_fields()-> record_info(fields, 'RegReq').
	
%% ----------------------------------------------------	
%% @doc	
%%	RegReqget函数	
%% @end	
%% ----------------------------------------------------	
get_user(#'RegReq'{user = V})->V.	
get_password(#'RegReq'{password = V})->V.	
	
%% ----------------------------------------------------	
%% @doc	
%%	RegReqset函数	
%% @end	
%% ----------------------------------------------------	
set_user(Record,V)->
	Record#'RegReq'{user = V}.	
set_password(Record,V)->
	Record#'RegReq'{password = V}.	
