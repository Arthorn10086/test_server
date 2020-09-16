-module(rd2mod).

%%%=======================STATEMENT====================
-description("rd2mod").
%%%=======================EXPORT=======================
-export([make_mod/3, write_head/2]).

-define(AUTHOR, "arthorn10086").
-define(COPYRIGHT, 'arthorn10086').
%%%=================EXPORTED FUNCTIONS=================
%% ----------------------------------------------------
%% @doc
%%  生成模块
%% @end
%% ----------------------------------------------------
make_mod(ToFile, Mod, RDL) ->
    {RecordAnno, ExportL, Func} = make_export(RDL),
    Forms1 = [{attribute, 1, module, Mod},
        {attribute, 2, description, atom_to_list(Mod)},
        {attribute, 3, copyright, ?COPYRIGHT},
        {attribute, 4, author, ?AUTHOR} |
        lists:keysort(2, ExportL)] ++
        [{attribute, RecordAnno, export_type, [{RDName, 0} || {RDName, _, _} <- RDL]}],
    RecordL = make_record(RDL, RecordAnno + 1),
    TypeAnno = element(2, lists:last(RecordL)) + 1,
    {EofAnno, Types} = lists:foldl(fun({RDName, _, _}, {I, Acc}) ->
        {I + 1, [{attribute, TypeAnno, type, {RDName, {type, TypeAnno, record, [{atom, TypeAnno, RDName}]}, []}} | Acc]}
    end, {TypeAnno, []}, RDL),
    Forms = Forms1 ++ RecordL ++ lists:reverse(Types) ++ [{eof, EofAnno}],
    String = erl_prettypr:format(erl_syntax:form_list(Forms)),
    write_head(ToFile, String),
    file:write_file(ToFile, "\t\n", [append]),
    file:write_file(ToFile, make_callout_line("EXPORTED FUNCTIONS"), [append]),
    lists:foreach(fun({RDName, GetF, SetF}) ->
        RDString = format_rd_name(RDName),
        {_, Fileds, _} = lists:keyfind(RDName, 1, RDL),
        file:write_file(ToFile, make_func_desc(RDString ++ "\tinit函数"), [append]),
        file:write_file(ToFile, "init_" ++ RDString ++ "()-> #'" ++ atom_to_list(RDName) ++ "'{}.\n", [append]),
        file:write_file(ToFile, "get_" ++ format_rd_name(RDName) ++ "_fields" ++ "()-> record_info(fields, '" ++ atom_to_list(RDName) ++ "').\n", [append]),
        file:write_file(ToFile, make_func_desc(RDString ++ "\tget函数"), [append]),
        lists:foreach(fun({{FN, _}, FieldN}) ->
            file:write_file(ToFile, io_lib:format("~s(#~p{~p = V})->V.\t\n", [FN, RDName, FieldN]), [append])
        end, lists:zip(GetF, Fileds)),
        file:write_file(ToFile, make_func_desc(RDString ++ "\tset函数"), [append]),
        lists:foreach(fun({{FN, _}, FieldN}) ->
            file:write_file(ToFile, io_lib:format("~s(Record,V)->\n\tRecord#~p{~p = V}.\t\n", [FN, RDName, FieldN]), [append])
        end, lists:zip(SetF, Fileds))
    end, Func),
    ok.


%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%  
%% @end
%% ----------------------------------------------------
make_export(RDL) ->
    lists:foldl(fun({RDName, Fields, _}, {Anno, Acc, Acc2}) ->
        {GetF, SetF} = lists:unzip([{{list_to_atom("get_" ++ atom_to_list(Field)), 1}, {list_to_atom("set_" ++ atom_to_list(Field)), 2}} || Field <- Fields]),
        {GetFL, Anno1} = split_list(GetF, [], Anno + 1),
        {FL, Anno2} = split_list(SetF, GetFL, Anno1),
        NAcc2 = [{RDName, GetF, SetF} | Acc2],
        {Anno2, [{attribute, Anno, export, [{list_to_atom("init_" ++ format_rd_name(RDName)), 0}, {list_to_atom("get_" ++ format_rd_name(RDName) ++ "_fields"), 0}]}
            | lists:reverse(FL)] ++ Acc, NAcc2}
    end, {5, [], []}, RDL).
split_list([], Acc, Anno) ->
    {Acc, Anno};
split_list([A, B, C, D, E, F, G, H | T], Acc, Anno) ->
    split_list(T, [{attribute, Anno, export, [A, B, C, D, E, F, G, H]} | Acc], Anno + 1);
split_list(T, Acc, Anno) ->
    split_list([], [{attribute, Anno, export, T} | Acc], Anno + 1).

make_record([{RDName, _, AfFieldDecls}], Anno) ->
    [make_record(RDName, AfFieldDecls, Anno)];
make_record([{RDName, _, AfFieldDecls} | T], Anno) ->
    [make_record(RDName, AfFieldDecls, Anno) | make_record(T, Anno + 1)].
make_record(RDName, AfFieldDecls, Anno) ->
    F = fun({typed_record_field, AfField, AbstractType}) ->
        AfField1 = handle_af_field(AfField, Anno),
        AbstractType1 = handle_abstract_type({AbstractType, Anno}),
        {typed_record_field, AfField1, AbstractType1};
        (AfField) ->
            handle_af_field(AfField, Anno)
    end,
    AfFieldDecls1 = lists:map(F, AfFieldDecls),
    {attribute, Anno, record, {RDName, AfFieldDecls1}}.

handle_af_field({record_field, _, {FT, _, FName}}, Anno) ->
    {record_field, Anno, {FT, Anno, FName}};
handle_af_field({record_field, _, {FT, _, FName}, AbstractExpr}, Anno) ->
    {record_field, Anno, {FT, Anno, FName}, AbstractExpr}.

handle_abstract_type({{'user_type', _, TypeName, AbstractTypeL}, Anno}) ->
    AbstractTypeL1 = lists:map(fun handle_abstract_type/1, [{A, Anno} || A <- AbstractTypeL]),
    case lists:keyfind(TypeName, 1, record_util:get_rd2mod()) of
        {_, Mod, _} ->
            {remote_type, Anno, [{atom, Anno, Mod}, {atom, Anno, TypeName}, AbstractTypeL1]};
        false ->
            {'user_type', Anno, TypeName, AbstractTypeL1}
    end;

handle_abstract_type({{'op', _, C, D}, Anno}) ->
    {'op', Anno, C, lists:map(fun handle_abstract_type/1, [{A, Anno} || A <- D])};
handle_abstract_type({{A, _, C, D}, Anno}) when is_list(D) ->
    {A, Anno, C, lists:map(fun handle_abstract_type/1, [{T, Anno} || T <- D])};
handle_abstract_type({{A, _, C, D}, Anno}) ->
    {A, Anno, C, handle_abstract_type({D, Anno})};



handle_abstract_type({{ann_type, _, C}, Anno}) ->
    {ann_type, Anno, lists:map(fun handle_abstract_type/1, [{A, Anno} || A <- C])};
handle_abstract_type({{A, _, C}, Anno}) ->
    {A, Anno, C};
handle_abstract_type({any, _Anno}) -> %%af_map_type af_tuple_type
    any.



write_head(File, String) ->
    [ModL | Lines] = string:tokens(String, "\n\t"),
    file:write_file(File, ModL ++ "\n"),
    [Desc, Copy, Author | ER] = Lines,
    file:write_file(File, make_callout_line("STATEMENT"), [append]),
    lists:foreach(fun(Line) ->
        file:write_file(File, Line ++ "\n", [append])
    end, [Desc, Copy, Author]),
    {ExportL, ExportType, TypeS, RecordL} = sizer_line(ER),
    file:write_file(File, make_callout_line("EXPORT"), [append]),
    lists:foreach(fun(Export) -> file:write_file(File, Export ++ "\n", [append]) end, ExportL),
    [file:write_file(File, EType ++ "\n", [append]) || EType <- ExportType],
    file:write_file(File, make_callout_line("RECORD"), [append]),
    lists:foreach(fun(Record1) ->
        [H1, H2 | Rest] = string:tokens(Record1, ","),
        file:write_file(File, H1 ++ "," ++ lists:sublist(H2, 1), [append]),
        lists:foreach(fun(L) ->
            [AA | _] = string:tokens(L, ":="),
            Bool =
                try
                    list_to_existing_atom(string:strip(AA))
                catch
                    _:_ ->
                        true
                end,
            Bool2 = lists:last(L) =:= $.,
            if
                Bool2 ->
                    file:write_file(File, "\n\t" ++ string:strip(L) ++ "\n", [append]);
                Bool ->
                    file:write_file(File, string:strip(L) ++ ",", [append]);
                true ->
                    file:write_file(File, "\n\t" ++ string:strip(L) ++ ",", [append])
            end
        end, [string:substr(H2, 2) | Rest])
    end, RecordL),
    file:write_file(File, make_callout_line("TYPE"), [append]),
    [file:write_file(File, Type ++ "\n", [append]) || Type <- TypeS],
    ok.




sizer_line(L) ->
    sizer_line(L, [], [], [], []).
sizer_line([], ExportL, ExportTypeL, TypeL, RecordL) ->
    {lists:reverse(ExportL), lists:reverse(ExportTypeL), lists:reverse(TypeL), lists:reverse(RecordL)};
sizer_line([H | _T] = L, ExportL, ExportTypeL, TypeL, RecordL) ->
    case lists:sublist(H, 7) == "-export" of
        true ->
            {Add, NT} = sizer_end(L, []),
            sizer_line(NT, [Add | ExportL], ExportTypeL, TypeL, RecordL);
        false ->
            case lists:sublist(H, 12) == "-export_type" of
                true ->
                    {Add, NT} = sizer_end(L, []),
                    sizer_line(NT, ExportL, [Add | ExportTypeL], TypeL, RecordL);
                false ->
                    case lists:sublist(H, 5) == "-type" of
                        true ->
                            {Add, NT} = sizer_end(L, []),
                            sizer_line(NT, ExportL, ExportTypeL, [Add | TypeL], RecordL);
                        false ->
                            {Add, NT} = sizer_end(L, []),
                            sizer_line(NT, ExportL, ExportTypeL, TypeL, [Add | RecordL])
                    end
            end
    end.

sizer_end([H | T], Acc) ->
    case lists:last(H) =:= $. of
        true ->
            {Acc ++ H, T};
        false ->
            sizer_end(T, Acc ++ H)
    end.



format_rd_name(Name) when is_atom(Name) ->
    format_rd_name(atom_to_list(Name));
format_rd_name(Name) when is_list(Name) ->
    case string:tokens(Name, ".") of
        [V] ->
            V;
        L ->
            string:join(L, "_")
    end.


make_func_desc(Desc) ->
    "\t\n%% ----------------------------------------------------\t\n%% @doc\t\n%%\t" ++ xmerl_ucs:to_utf8(Desc) ++ "\t\n%% @end\t\n%% ----------------------------------------------------\t\n".
make_callout_line(Desc) when is_atom(Desc) ->
    "%%%=======================" ++ atom_to_list(Desc) ++ "====================\n";
make_callout_line(Desc) when is_list(Desc) ->
    "%%%=======================" ++ Desc ++ "====================\n".