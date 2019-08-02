%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.10.0

-ifndef(login_pb).
-define(login_pb, true).

-define(login_pb_gpb_version, "4.10.0").

-ifndef('LOGINREQ_PB_H').
-define('LOGINREQ_PB_H', true).
-record('LoginReq',
        {user                   :: iodata(),        % = 1
         password               :: iodata()         % = 2
        }).
-endif.

-ifndef('LOGINRESP_PB_H').
-define('LOGINRESP_PB_H', true).
-record(loginResp,
        {now_ms                 :: non_neg_integer(), % = 1, 32 bits
         role_info = []         :: [login_pb:'RoleInfo'()] | undefined % = 2
        }).
-endif.

-ifndef('ROLEINFO_PB_H').
-define('ROLEINFO_PB_H', true).
-record('RoleInfo',
        {role_uid               :: non_neg_integer(), % = 1, 32 bits
         role_name              :: iodata(),        % = 2
         role_lv                :: non_neg_integer(), % = 3, 32 bits
         profession             :: non_neg_integer(), % = 4, 32 bits
         figure                 :: non_neg_integer() % = 5, 32 bits
        }).
-endif.

-endif.