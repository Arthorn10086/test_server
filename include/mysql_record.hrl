%%%-------------------------------------------------------------------
%%% @author yhw,yanghaiwei@youkia.net
%%% @copyright (C) 2020, youkia,www.youkia.net
%%% @doc
%%%
%%% @end
%%% Created : 04. 九月 2020 14:27
%%%-------------------------------------------------------------------
-author("yhw,yanghaiwei@youkia.net").
-record(user, {
    user_id = 0 :: integer(),
    password = "" :: list(),
    role
}).
