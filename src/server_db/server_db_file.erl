-module(server_db_file).
-author("yhw").

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([lock/5, update/5, delete/5, get/5]).

-define(SERVER, ?MODULE).
-define(INTERVAL, 1000).
-include("../../include/server.hrl").

-record(state, {db_name, key, interval, cahce_size, cahce_time, key_ets, ets, fields}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(ID, Name, Options) ->
    gen_server:start_link({local, ID}, ?MODULE, [{Name, Options}], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([{Name, Options}]) ->
    FieldsSql = server_db_lib:get_fields_sql(Name),
    {_, _, Fields} = mysql_poolboy:query(?POOLNAME, FieldsSql),
    FieldL = [list_to_atom(binary:bin_to_list(B)) || B <- lists:flatten(Fields)],
    case FieldL of
        [] ->
            {stop, 'none_table'};
        FieldL ->
            {_, InterVal} = lists:keyfind('interval', 1, Options),
            {_, KeyName, KeyType} = lists:keyfind('key', 1, Options),
            Ets = ets:new(?MODULE, ['protected', 'set']),
            KeyEts = ets:new(?MODULE, ['protected', 'ordered_set']),
            erlang:send_after(InterVal, self(), 'batch_write'),
            {ok, #state{db_name = Name, interval = InterVal, key = {KeyName, KeyType}, ets = Ets, key_ets = KeyEts, fields = lists:reverse(FieldL)}, 0}
    end.


handle_call({Action, Key, Value, Vsn, LockTime, Locker}, _From, State) ->
    Reply = action(State, Action, Key, Value, Vsn, Locker, LockTime, time_lib:now_millisecond()),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(timeout, #state{db_name = DBName, key_ets = Ets, key = {KeyName, KeyType}} = State) ->
    SQL = server_db_lib:get_all_key_sql(DBName, KeyName, KeyType),
    {_, _, KeyList} = mysql_poolboy:query(?POOLNAME, SQL),
    lists:foreach(fun([Key]) -> ets:insert(Ets, {Key}) end, KeyList),
    {noreply, State};
handle_info(batch_write, #state{db_name = DBName, ets = Ets, key = {KeyName, KeyType}, interval = InterVal, fields = Fields} = State) ->
    erlang:send_after(InterVal, self(), batch_write),
    batch_write(DBName, Ets, Fields, KeyName, KeyType),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, #state{db_name = DBName, ets = Ets, fields = Fields, key = {KeyName, KeyType}}) ->
    batch_write(DBName, Ets, Fields, KeyName, KeyType),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_replace_prepare(DBName, Fields) ->
    {[_ | R], [_ | T]} = lists:foldl(fun(Field, {Acc1, Acc2}) ->
        {[$, | server_db_lib:term_to_string(Field) ++ Acc1], [$,, $? | Acc2]}
    end, {"", []}, lists:reverse(Fields, [])),
    "REPLACE INTO " ++ server_db_lib:term_to_string(DBName) ++ " (" ++ R ++ ") VALUES (" ++ T ++ ");".
get_delete_prepare(DBName, KeyName, KeyType) ->
    case KeyType of
        integer ->
            "DELETE FROM " ++ atom_to_list(DBName) ++ " WHERE " ++ atom_to_list(KeyName) ++ "=?;";
        string ->
            "DELETE FROM " ++ atom_to_list(DBName) ++ " WHERE " ++ atom_to_list(KeyName) ++ " LIKE '%?%';"
    end.

to_params(Fields, KVL) ->
    [element(2, lists:keyfind(Field, 1, KVL)) || Field <- Fields].

batch_write(DBName, Ets, Fields, KeyName, KeyType) ->
    ReplacePrepare = get_replace_prepare(DBName, Fields),
    DelPrepare = get_delete_prepare(DBName, KeyName, KeyType),
    Conn = poolboy:checkout(?POOLNAME2),
    {ok, I1} = mysql:prepare(Conn, ReplacePrepare),
    {ok, I2} = mysql:prepare(Conn, DelPrepare),
    F = fun({_, _, _, 0}, _Acc) ->
        ok;
        ({Key, Maps, Time, _Version}, _Acc) ->
            Params = to_params(Fields, maps:to_list(Maps)),
            mysql:execute(Conn, I1, Params),
            ets:insert(Ets, {Key, Maps, Time, 0}),
            ok;
        ({Key, 'delete'}, _Acc) ->
            mysql:execute(Conn, I2, Key),
            ets:delete(Ets, Key),
            ok
    end,
    ets:foldl(F, [], Ets),
    mysql:unprepare(Conn, I1),
    mysql:unprepare(Conn, I2),
    poolboy:checkin(?POOLNAME2, Conn),
    ok.



action(State, Action, Key, Value, Vsn, Locker, LockTime, MS) ->
    case erlang:get({'lock', Key}) of
        undefined when LockTime > 0 ->
            erlang:put({'lock', Key}, {Locker, MS + LockTime}),
            ?MODULE:Action(State, Key, Value, Vsn, MS);
        undefined ->
            ?MODULE:Action(State, Key, Value, Vsn, MS);
        {Locker, _} ->
            if
                LockTime =:= 0 ->
                    erlang:erase({'lock', Key});
                true ->
                    erlang:put({'lock', Key}, {Locker, MS + LockTime})
            end,
            ?MODULE:Action(State, Key, Value, Vsn, MS);
        {_Lock, EndTime} ->
            if
                EndTime < MS ->
                    erlang:put({'lock', Key}, {Locker, MS + LockTime}),
                    ?MODULE:Action(State, Key, Value, Vsn, MS);
                true ->
                    lock_error
            end
    end.


lock(_, _, _, _, _) ->
    ok.
get(State, Key, _, _, MS) ->
    #state{db_name = DBName, ets = Ets, key_ets = KeyEts, key = {KeyName, KeyType}, fields = Fields} = State,
    case ets:lookup(KeyEts, Key) of
        [] ->
            none;
        _ ->
            case ets:lookup(Ets, Key) of
                [] ->
                    SQL = server_db_lib:get_query_by_key(DBName, KeyName, KeyType, Key),
                    case mysql_poolboy:query(?POOLNAME, SQL) of
                        {_, _, []} ->
                            none;
                        {ok, _FieldL, [DataL]} ->
                            ZipL = lists:zip(Fields, DataL),
                            Maps = maps:from_list(ZipL),
                            ets:insert(Ets, {Key, Maps, MS, 0}),
                            {ok, Maps, 0, MS}
                    end;
                [{Key, 'delete'}] ->
                    none;
                [{Key, Maps, Time, Version}] ->
                    {ok, Maps, Version, Time}
            end
    end.
update(State, Key, Value, Vsn, MS) ->
    #state{ets = Ets, key_ets = KeyEts} = State,
    case ets:lookup(Ets, Key) of
        [] ->
            ets:insert(KeyEts, {Key}),
            ets:insert(Ets, {Key, Value, MS, 1});
        [{_, _, _, Vsn}] ->
            ets:insert(Ets, {Key, Value, MS, Vsn + 1});
        [{_, _, _, _OldVsn}] ->
            {'error', 'version_error'}
    end.
delete(State, Key, _, _, _) ->
    #state{ets = Ets, key_ets = KeyEts} = State,
    ets:delete(KeyEts, Key),
    ets:insert(Ets, {Key, 'delete'}).
