-module(record_util).

%%%=======================STATEMENT====================
-description("record_util").
-author("arthorn10086").

%%%=======================EXPORT=======================
-export([rd2mod/0, rd2mod/2, rd2mod/3, get_rd2mod/0, map2record/2, fields_info/1]).

%%%===================EXPORTED FUNCTIONS==================
%%{record_name,mod,outpath}
-define(TAB2RD, [
    {'LoginReq', 'login', "./src/record/"},
    {'loginResp.RoleInfo', 'login', "./src/record/"},
    {'loginResp', 'login', "./src/record/"},
    {'RegResp', 'register', "./src/record/"},
    {'RegReq', 'register', "./src/record/"}
]).
-define(INHEAD, "./include/record.hrl").
-define(TEMPLATE, rd2mod).
%% ----------------------------------------------------
%% @doc
%%      rd2mod
%% @end
%% ----------------------------------------------------
get_rd2mod() ->
    ?TAB2RD.

rd2mod() ->
    rd2mod([], ?INHEAD, ?TEMPLATE).

rd2mod(RDL, InHead) when is_list(InHead) ->
    rd2mod(RDL, InHead, ?TEMPLATE).

rd2mod(RDL, InHead, Template) ->
    {ok, Forms} = epp:parse_file(InHead, []),
    AfRecordDeclL = filter_rd(Forms, []),
    HandleRecordL = case length(RDL) =:= 0 of
        true ->
            AfRecordDeclL;
        false ->
            lists:filter(fun({RName, _}) -> lists:member(RName, RDL) end, AfRecordDeclL)
    end,
    RFl = analysis(HandleRecordL, []),
    RDFL = lists:foldl(fun({RDName, _Fileds, _AfFieldDecls} = RDInfo, Acc) ->
        {_, Mod, OutPath} = lists:keyfind(RDName, 1, ?TAB2RD),
        case lists:keyfind(Mod, 1, Acc) of
            false ->
                ToFile = OutPath ++ atom_to_list(Mod) ++ ".erl",
                [{Mod, ToFile, [RDInfo]} | Acc];
            {_, ToF, L} ->
                lists:keyreplace(Mod, 1, Acc, {Mod, ToF, [RDInfo | L]})
        end
    end, [], RFl),
    lists:foreach(fun({Mod, ToFile, FRDL}) ->
        Template:make_mod(ToFile, Mod, lists:reverse(FRDL))
    end, RDFL).

%% ----------------------------------------------------
%% @doc
%%      map2rd
%% @end
%% ----------------------------------------------------
map2record(RecordName, MapData) when is_list(MapData) ->
    [map2record(RecordName, SubData) || SubData <- MapData];
map2record(RecordName, MapData) when is_map(MapData) ->
    try
        Fields = fields_info(RecordName),
        Values = lists:map(fun(Field) ->
            map2record(Field, maps:get(Field, MapData, undefined))
        end, Fields),
        list_to_tuple([RecordName | Values])
    catch
        _:_ ->
            MapData
    end;
map2record(_, Data) ->
    Data.


%% ----------------------------------------------------
%% @doc
%%      record方法名 .替换成_
%% @end
%% ----------------------------------------------------
format_rd_name(Name) when is_atom(Name) ->
    format_rd_name(atom_to_list(Name));
format_rd_name(Name) when is_list(Name) ->
    case string:tokens(Name, ".") of
        [V] ->
            V;
        L ->
            string:join(L, "_")
    end.
%%%===================EXPORTED FUNCTIONS==================
%% ----------------------------------------------------
%% @doc
%%      解析record
%% @end
%% ----------------------------------------------------
filter_rd([], Acc) ->
    Acc;
filter_rd([{attribute, _Line, record, Record} | T], Acc) ->
    filter_rd(T, [Record | Acc]);
filter_rd([_ | T], Acc) ->
    filter_rd(T, Acc).

analysis([], Acc) ->
    Acc;
analysis([AfFieldDecl | T], Acc) ->
    Af = analysis_rd(AfFieldDecl),
    analysis(T, [Af | Acc]).

analysis_rd({RDName, Fields}) ->
    {RDName, analysis_rd(Fields, []), Fields}.
analysis_rd([], Acc1) ->
    lists:reverse(Acc1);
analysis_rd([{'typed_record_field', AfField, AbstractType} | T], Acc1) ->
    FieldInfo = analysis_field(AfField, AbstractType),
    analysis_rd(T, [FieldInfo | Acc1]);
analysis_rd([AfField | T], Acc1) ->
    FieldInfo = analysis_field(AfField, []),
    analysis_rd(T, [FieldInfo | Acc1]).

analysis_field(AfField, _AbstractType) ->
    {_FieldType, _Line, Field} = element(3, AfField),
    Field.



fields_info(RecordName) ->
    Mod = case lists:keyfind(RecordName, 1, ?TAB2RD) of
        false ->
            RecordName;
        {_, M, _} ->
            M
    end,
    Func = list_to_atom("get_" ++ format_rd_name(RecordName) ++ "_fields"),
    Mod:Func().



