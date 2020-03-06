-module(statistics_tool).
-author("yhw").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(WRITE_INTERVAL, 3600).%%汇总间隔 秒
-define(DIF, 10).
-define(FILE_ROOT, "./log").

-record(state, {ets, write_interval, file}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    Ets = ets:new(?MODULE, [set, protected, {write_concurrency, true}]),
    Now = time_lib:now_second(),
    ZeroTime = time_lib:get_zero_second(),
    N = (Now - ZeroTime) rem ?WRITE_INTERVAL,
    Ref = erlang:start_timer((?WRITE_INTERVAL - N) * 1000, self(), 'write_file'),
    {Date, FileName} = get_file_name({none, none}),
    {ok, #state{ets = Ets, write_interval = Ref, file = {Date, FileName}}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'protocol', Cmd}, #state{ets = Ets} = State) ->
    case ets:lookup(Ets, Cmd) of
        [] ->
            ets:insert(Ets, {Cmd, 1});
        [{_, N}] ->
            ets:insert(Ets, {Cmd, N + 1})
    end,
    {noreply, State};
handle_info({'timeout', _Ref, 'write_file'}, #state{ets = Ets, file = FileInfo} = State) ->
    {Date, FileName} = get_file_name(FileInfo),
    Info = format(Ets),
    try
        file:write_file(FileName, io_lib:format("~p.~n~n", [Info]), [append])
    catch
        E1:E2 ->
            lager:log(error, self(), [{data, Info}, {strace, lager:pr_stacktrace(erlang:get_stacktrace(), {E1, E2})}])
    end,
    Now = time_lib:now_second(),
    ZeroTime = time_lib:get_zero_second(),
    N = (Now - ZeroTime) rem ?WRITE_INTERVAL,
    Ref = erlang:start_timer((?WRITE_INTERVAL - N) * 1000, self(), 'write_file'),
    {noreply, State#state{file = {Date, FileName}, write_interval = Ref}};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_file_name({OldDate, F}) ->
    {{Y, M, D}, _Time} = calendar:now_to_local_time(os:timestamp()),
    if
        OldDate =:= {Y, M, D} ->
            {OldDate, F};
        true ->
            Name = "statistics-" ++ integer_to_list(Y) ++ "-" ++ integer_to_list(M) ++ "-" ++ integer_to_list(D),
            {{Y, M, D}, filename:join(?FILE_ROOT, Name)}
    end.

format(Ets) ->
    {_, Time} = calendar:now_to_local_time(os:timestamp()),
    Info = ets:tab2list(Ets),
    ets:delete_all_objects(Ets),
    {Time, Info}.