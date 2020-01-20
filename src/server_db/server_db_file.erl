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

-export([lock/5, update/5, delete/5, get/5, batch_write/5]).

-define(SERVER, ?MODULE).
-define(CAHCE_INTERVAL, 30000).
-include("../../include/server.hrl").

-record(state, {db_name, key, interval, cahce_tactics, cahce_size, cahce_time, key_ets, ets, fields, cahce_ref, write_ref}).

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
    {ok, proc_lib:spawn_link(?MODULE, init, [{ID, Name, Options}])}.
%%    gen_server:start_link({local, ID}, ?MODULE, [{Name, Options}], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({ID, Name, Options}) ->
    register(ID, self()),
    %%拿到所有字段
    FieldsSql = server_db_lib:get_fields_sql(Name),
    {_, _, Fields} = mysql_poolboy:query(?POOLNAME, FieldsSql),
    FieldL = [list_to_atom(binary:bin_to_list(B)) || B <- lists:flatten(Fields)],
    case FieldL of
        [] ->%%表不存在
            {stop, 'none_table'};
        FieldL ->
            {_, InterVal} = lists:keyfind('interval', 1, Options),%秒
            {_, KeyName, KeyType} = lists:keyfind('key', 1, Options),
            {_, Tactics} = lists:keyfind('cache_tactics', 1, Options),
            {_, CacheTime} = lists:keyfind('cache_time', 1, Options),
            {_, CachaSize} = lists:keyfind('cache_size', 1, Options),%KB
            Ets = ets:new(?MODULE, ['protected', 'set']),
            KeyEts = ets:new(?MODULE, ['protected', 'ordered_set']),
            %%加载所有Key
            SQL = server_db_lib:get_all_key_sql(Name, KeyName, KeyType),
            {_, _, KeyList} = mysql_poolboy:query(?POOLNAME, SQL),
            lists:foreach(fun([Key]) -> ets:insert(KeyEts, {Key}) end, KeyList),
            %%缓存策略
            BehindBool = is_behind(Tactics),
            Ref = if
                BehindBool ->%%缓存延时批量写
                    Now = time_lib:now_second(),
                    ZeroTime = time_lib:get_zero_second(),
                    DayTime = Now-ZeroTime,
                    NextTime = trunc(DayTime / InterVal) * InterVal + InterVal,
                    DifTime = NextTime-DayTime,
                    erlang:start_timer(DifTime * 1000, self(), 'batch_write');
                true ->
                    ok
            end,
            CahceRef = erlang:start_timer(?CAHCE_INTERVAL, self(), 'refresh_cahce'),
            SizeOut = CachaSize * 1024 div erlang:system_info(wordsize),
            gen_server:enter_loop(?MODULE, [],
                #state{db_name = Name, interval = InterVal, key = {KeyName, KeyType}, ets = Ets, key_ets = KeyEts,
                    fields = FieldL, cahce_time = CacheTime, cahce_size = SizeOut, cahce_tactics = Tactics, write_ref = Ref, cahce_ref = CahceRef}, {local, ID})
    end.


handle_call({Action, Key, Value, Vsn, LockTime, Locker}, _From, State) ->
    Reply = action(State, Action, Key, Value, Vsn, Locker, LockTime, time_lib:now_millisecond()),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({batch_write}, #state{db_name = DBName, ets = Ets, fields = Fields, key = {KeyName, KeyType}} = State) ->
    batch_write(DBName, Ets, Fields, KeyName, KeyType),
    io:format("batch_write_ok =~p~n", [DBName]),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%%持久化
handle_info({timeout, Ref, 'batch_write'}, #state{db_name = DBName, ets = Ets, key = {KeyName, KeyType}, cahce_tactics = Tactics, interval = InterVal, fields = Fields, write_ref = Ref} = State) ->
    case Tactics of
        write_behind ->
            NRef = erlang:start_timer(InterVal * 1000, self(), 'batch_write'),
            batch_write(DBName, Ets, Fields, KeyName, KeyType),
            {noreply, State#state{write_ref = NRef}};
        write_through ->
            {noreply, State}
    end;
%%刷新缓存
handle_info({timeout, Ref, 'refresh_cahce'}, #state{cahce_ref = Ref} = State) ->
    MS = time_lib:now_millisecond(),
    NewRef = erlang:start_timer(?CAHCE_INTERVAL, self(), 'refresh_cahce'),
    remove_cold_data(State, MS),
    {noreply, State#state{cahce_ref = NewRef}};
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
%%操作
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
                [{Key, Maps, Time, Version}] ->
                    ets:update_element(Ets, Key, {3, MS}),
                    {ok, Maps, Version, Time}
            end
    end.
update(State, Key, Value, Vsn, MS) ->
    #state{cahce_tactics = Tactics, ets = Ets, key_ets = KeyEts} = State,
    Bool = is_behind(Tactics),
    case ets:lookup(Ets, Key) of
        [] ->%%新增
            if
                Bool ->
                    ok;
                true ->
                    insert_mysql_value(State, Value)
            end,
            %%修改缓存
            ets:insert(KeyEts, {Key}),
            ets:insert(Ets, {Key, Value, MS, 1});
        [{_, OldValue, _, Vsn}] ->%%修改
            if
                Bool ->
                    ok;
                true ->
                    update_mysql(State, Key, Value, OldValue)
            end,
            %修改缓存
            ets:insert(Ets, {Key, Value, MS, Vsn + 1});
        [{_, _, _, _OldVsn}] ->
            {'error', 'version_error'}
    end.
delete(State, Key, _, _, _) ->
    #state{cahce_tactics = Tactics, ets = Ets, key_ets = KeyEts} = State,
    case Tactics of
        write_behind ->
            ets:delete(KeyEts, Key),
            ets:insert(Ets, {Key, 'delete', 0, 1}),
            ok;
        write_through ->
            delete_mysql_value(State, Key),
            ets:delete(KeyEts, Key),
            ets:delete(Ets, Key),
            %%数据库删除
            ok
    end.


%%mysql插入
insert_mysql_value(#state{db_name = DBName, fields = Fields}, Value) ->
    mysql_poolboy:query(?POOLNAME, get_replace_prepare(DBName, Fields), tuple_to_list(Value)).

%修改指定字段的值
update_mysql(#state{db_name = DBName, fields = Fields, key = {KeyName, KeyType}}, Key, Value, OldValue) ->
    {UPFields, UPValues} = get_update_values(Fields, Value, OldValue),
    Sql = get_update_sql(DBName, KeyName, KeyType, UPFields),
    mysql_poolboy:query(?POOLNAME, Sql, UPValues ++ [Key]).

%%mysql删除
delete_mysql_value(#state{db_name = DBName, key = {KeyName, KeyType}}, Key) ->
    mysql_poolboy:query(?POOLNAME, get_delete_prepare(DBName, KeyName, KeyType), [Key]).


%%判断缓存是否是behind模式
is_behind(Tactics) ->
    Tactics =:= write_behind.


%%获得变化的字段和新值
get_update_values(Fields, Value, OldValue) ->
    {UpK, UpV} = lists:foldl(fun(Field, {R1, R2}) ->
        NV = maps:get(Field, Value),
        case NV =:= maps:get(Field, OldValue) of
            true ->
                {R1, R2};
            false ->
                {[Field | R1], [NV | R2]}
        end
    end, {[], []}, Fields),
    {lists:reverse(UpK), lists:reverse(UpV)}.


%%组装Update语句
get_update_sql(DBName, KeyName, KeyType, Fields) ->
    [_ | R] = lists:flatten(lists:map(fun(Field) ->
        "," ++ atom_to_list(Field) ++ "= ?"
    end, Fields)),
    Clause = case KeyType of
        integer ->
            "=?";
        _ ->
            "Like ?"
    end,
    "UPDATE " ++ atom_to_list(DBName) ++ " SET " ++ R ++ " WHERE " ++ atom_to_list(KeyName) ++ Clause.


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
            "DELETE FROM " ++ atom_to_list(DBName) ++ " WHERE " ++ atom_to_list(KeyName) ++ " LIKE ?;"
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
        ({Key, 'delete', 0, 1}, _Acc) ->
            mysql:execute(Conn, I2, [Key]),
            ets:delete(Ets, Key),
            ok;
        ({Key, Maps, _Time, _Version}, _Acc) ->
            Params = to_params(Fields, maps:to_list(Maps)),
            mysql:execute(Conn, I1, Params),
            ets:update_element(Ets, Key, {4, 0}),
            ok
    end,
    ets:foldl(F, [], Ets),
    mysql:unprepare(Conn, I1),
    mysql:unprepare(Conn, I2),
    poolboy:checkin(?POOLNAME2, Conn),
    ok.


remove_cold_data(State, MS) ->
    #state{ets = Ets, cahce_tactics = Tactics, cahce_time = CTime} = State,
    case is_behind(Tactics) of
        true ->%清理缓存的时候要同步数据库
            remove_time_out1(State, MS - CTime),
            remove_overflow(State, MS);
        false ->
            %%过期数据
            remove_time_out(Ets, MS - CTime),
            %%溢出数据
            remove_overflow(State, MS)

    end.


%% 10W数据筛选删除5W条数据   select_delete 12毫秒    ets:foldl 300毫秒      select再lists:foreach删除 80毫秒

%%如果刷硬盘的间隔时间小于缓存过期时间直接删除冷数据
remove_time_out(Ets, MS) ->
    ets:select_delete(Ets, [{{'$1', '$2', '$3', '$4'}, [{'=<', '$3', MS}], [true]}]).

%%如果刷硬盘的间隔时间大于缓存过期时间,清理时要刷到数据库
remove_time_out1(State, MS) ->
    #state{db_name = DBName, key = {KeyName, KeyType}, fields = Fields, ets = Ets} = State,
    UpdateL = ets:select(Ets, [{{'$1', '$2', '$3', '$4'}, [{'=<', '$3', MS}, {'>', '$4', 0}], [{{'$1', '$2'}}]}]),
    remove_time_out(Ets, MS),
    ReplacePrepare = get_replace_prepare(DBName, Fields),
    DelPrepare = get_delete_prepare(DBName, KeyName, KeyType),
    Conn = poolboy:checkout(?POOLNAME2),
    {ok, I1} = mysql:prepare(Conn, ReplacePrepare),
    {ok, I2} = mysql:prepare(Conn, DelPrepare),
    lists:foreach(fun({Key, 'delete'}) ->
        mysql:execute(Conn, I2, [Key]);
        ({_, Maps}) ->
            Params = to_params(Fields, maps:to_list(Maps)),
            mysql:execute(Conn, I1, Params)
    end, UpdateL),
    mysql:unprepare(Conn, I1),
    mysql:unprepare(Conn, I2),
    poolboy:checkin(?POOLNAME2, Conn),
    ok.

%%溢出
remove_overflow(State, MS) ->
    #state{ets = Ets, cahce_size = SizeOut, cahce_time = TimeOut, cahce_tactics = Tactics} = State,
    Info = ets:info(Ets),
    {_, Size} = lists:keyfind(size, 1, Info),
    {_, Memory} = lists:keyfind(memory, 1, Info),
    %估算预计多少条数据缓存上限
    Size1 = trunc(SizeOut / Memory * SizeOut),
    %%保留上限的2/3
    RetainSize = Size1 * 2 div 3,
    Range = TimeOut div 10,
    if
        Memory > SizeOut andalso Size > 0 ->
            %%获得所有数据时间分布
            TimeRange = ets:foldl(fun({_, _, _, T}, R) ->
                I = (MS - T) div Range + 1,
                erlang:setelement(I, R, erlang:element(I, R) + 1)
            end, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, Ets),
            %%计算删除的时间点
            Time = get_remove_time(TimeRange, Range, Size - RetainSize, tuple_size(TimeRange), 0, 0),
            case is_behind(Tactics) of
                true ->
                    remove_time_out1(State, MS - Time);
                false ->
                    remove_time_out(Ets, MS - Time)
            end,
            ok;
        true ->
            ok
    end.

%计算开始移除的时间段
get_remove_time(_TimeRange, _Range, _RetainSize, 0, _OldCount, _NewCount) ->
    0;
get_remove_time(_TimeRange, Range, RetainSize, I, _OldCount, NewCount) when NewCount == RetainSize ->
    Range * I;
get_remove_time(_TimeRange, Range, RetainSize, I, OldCount, NewCount) when NewCount > RetainSize ->
    Time1 = Range * I,
    Time1 + trunc((1 - (RetainSize - OldCount) / (NewCount - OldCount)) * Range);
get_remove_time(TimeRange, Range, RetainSize, I, _OldCount, NewCount) ->
    Add = element(I, TimeRange),
    get_remove_time(TimeRange, Range, RetainSize, I - 1, NewCount, NewCount + Add).