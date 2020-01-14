-module(server_db_lib).

%%%=======================STATEMENT====================
-description("server_db_lib").
-author("yhw").

%%%=======================EXPORT=======================
-export([get_db_name/1, get_query_by_key/4, get_all_key_sql/3, get_fields_sql/1,
    term_to_string/1, string_to_term/1]).

%%%=======================INCLUDE======================
-include("../../include/server.hrl").
%%%=======================DEFINE======================

%%%=======================RECORD=======================

%%%=======================TYPE=========================
%%-type my_type() :: atom() | integer().


%%%=================EXPORTED FUNCTIONS=================
%% ----------------------------------------------------
%% @doc  
%%        获得表进程ID
%% @end
%% ----------------------------------------------------
get_db_name(Name) ->
    list_to_atom("server_db:" ++ atom_to_list(Name)).

%% ----------------------------------------------------
%% @doc
%%        获得表字段
%% @end
%% ----------------------------------------------------
get_fields_sql(Name) ->
    "select COLUMN_NAME from information_schema.COLUMNS where table_name = '"
        ++ atom_to_list(Name) ++ "' and table_schema = '" ++ atom_to_list(?DBNAME) ++ "';".

%% ----------------------------------------------------
%% @doc
%%       查询语句
%% @end
%% ----------------------------------------------------
get_query_by_key(DBName, KeyName, KeyType, Key) ->
    case KeyType of
        integer ->
            "SELECT * FROM " ++ atom_to_list(DBName) ++ " WHERE " ++ atom_to_list(KeyName) ++ "=" ++ integer_to_list(Key) ++ ";";
        string ->
            "SELECT * FROM " ++ atom_to_list(DBName) ++ " WHERE " ++ atom_to_list(KeyName) ++ " LIKE '%" ++ xmerl_ucs:to_utf8(Key) ++ "%';"
    end.

%% ----------------------------------------------------
%% @doc
%%       查询所有Key
%% @end
%% ----------------------------------------------------
get_all_key_sql(DBName, KeyName, _KeyType) ->
    "SELECT " ++ atom_to_list(KeyName) ++ " FROM " ++ atom_to_list(DBName) ++ " WHERE " ++ atom_to_list(KeyName) ++ ";".
%% ----------------------------------------------------
%% @doc
%%       转化成sql string
%% @end
%% ----------------------------------------------------
term_to_string(Term) when is_atom(Term) ->
    atom_to_list(Term);
term_to_string(Term) when is_integer(Term) ->
    integer_to_list(Term);
term_to_string(Term) when is_float(Term) ->
    float_to_list(Term);
term_to_string(Term) when is_tuple(Term); is_list(Term); is_binary(Term); is_map(Term) ->
    binary_to_list(unicode:characters_to_binary(io_lib:format("'~p.'", [Term])));
term_to_string(Term) when is_pid(Term) ->
    erlang:pid_to_list(Term);
term_to_string(Term) when is_function(Term) ->
    erlang:fun_to_list(Term).
%% ----------------------------------------------------
%% @doc
%%       sql string 转化为 erlang term
%% @end
%% ----------------------------------------------------
string_to_term(Text) ->
    case erl_scan:string(Text) of
        {ok, Tokens, _EndLine} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, Expr} ->
                    Expr;
                E ->
                    throw({'parse_error2', E, Text})
            end;
        E ->
            throw({'parse_error1', E, Text})
    end.


%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
