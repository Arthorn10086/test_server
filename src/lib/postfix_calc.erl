-module(postfix_calc).

%%%=======================STATEMENT====================
-description("cacl_lib").
-author("arthorn").

%%%=======================EXPORT=======================
-export([mid2postfix/1, calc_postfix/1, postfix_view/1]).

%%%=======================INCLUDE======================

%%%=======================DEFINE======================

%%%=======================RECORD=======================

%%%=======================TYPE=========================
%%-type my_type() :: atom() | integer().


%%%=================EXPORTED FUNCTIONS=================
%% ----------------------------------------------------
%% @doc  
%%     中缀表达式转后缀表达式
%% @end
%% ----------------------------------------------------
%%初始化两个列表S1，S2，从左边边开始遍历表达式
%%遇到运算符放入S1,运算数放入S2
mid2postfix(Str) ->
    mid2postfix(Str, [], [], []).

mid2postfix([$) | T], S1, S2, Temp) ->
    {NS1, NS2} = pop($), S1, operand(Temp, S2)),
    mid2postfix(T, NS1, NS2, []);
mid2postfix([$( | T], S1, S2, Temp) ->
    {NS1, NS2} = pop($(, S1, operand(Temp, S2)),
    mid2postfix(T, NS1, NS2, []);
mid2postfix([$+ | T], S1, S2, Temp) ->
    {NS1, NS2} = pop($+, S1, operand(Temp, S2)),
    mid2postfix(T, NS1, NS2, []);
mid2postfix([$- | T], S1, S2, Temp) ->
    {NS1, NS2} = pop($-, S1, operand(Temp, S2)),
    mid2postfix(T, NS1, NS2, []);
mid2postfix([$* | T], S1, S2, Temp) ->
    {NS1, NS2} = pop($*, S1, operand(Temp, S2)),
    mid2postfix(T, NS1, NS2, []);
mid2postfix([$/ | T], S1, S2, Temp) ->
    {NS1, NS2} = pop($/, S1, operand(Temp, S2)),
    mid2postfix(T, NS1, NS2, []);
mid2postfix([H | T], S1, S2, Temp) ->
    mid2postfix(T, S1, S2, [H | Temp]);
mid2postfix([], S1, S2, Temp) ->
    lists:reverse(lists:foldl(fun(I, Acc) -> [{'operator', I} | Acc] end, operand(Temp, S2), S1)).
%% ----------------------------------------------------
%% @doc
%%     后缀表达式展示
%% @end
%% ----------------------------------------------------
postfix_view(List) ->
    lists:flatten([begin
        if
            Type =:= 'operator' ->
                V;
            true ->
                integer_to_list(V)++" "
        end
    end || {Type, V} <- List]).
%% ----------------------------------------------------
%% @doc
%%     后缀表达式计算
%% @end
%% ----------------------------------------------------
%% 从左至右扫描表达式，遇到数字时，将数字压入堆栈，
%% 遇到运算符时，弹出栈顶的两个数，用运算符对它们做相应的计算（栈顶元素 op 次顶元素），并将结果入栈；
%% 重复上述过程
calc_postfix(Str) ->
    calc_postfix(Str, []).
calc_postfix([{'operator', H} | Str], L) ->
    L1 = calc(H, L),
    calc_postfix(Str, L1);
calc_postfix([{'operand', H} | Str], L) ->
    calc_postfix(Str, [H | L]);
calc_postfix([], L) ->
    L.


%%%===================LOCAL FUNCTIONS==================
%% ----------------------------------------------------
%% @doc  
%%      优先级
%% @end
%% ----------------------------------------------------
pop_priority($*, $+) ->
    true;
pop_priority($*, $-) ->
    true;
pop_priority($/, $+) ->
    true;
pop_priority($/, $-) ->
    true;
pop_priority(_, $() ->
    true;
pop_priority(_, $)) ->
    true;
pop_priority(_, _) ->
    false.
%% ----------------------------------------------------
%% @doc
%%      计算
%% @end
%% ----------------------------------------------------
calc($*, [V1, V2 | L]) ->
    [V2 * V1 | L];
calc($/, [V1, V2 | L]) ->
    [V2 / V1 | L];
calc($+, [V1, V2 | L]) ->
    [V2 + V1 | L];
calc($-, [V1, V2 | L]) ->
    [V2 - V1 | L].
%% ----------------------------------------------------
%% @doc
%%      栈操作
%% @end
%% ----------------------------------------------------
%%遇到右括号，直接将操作符栈S1元素弹出放入S2，直到遇到左括号
%%遇到其他操作符，从S1栈中弹出元素直到遇到更低优先级的元素为止
pop($), [$( | S1], S2) ->
    {S1, S2};
pop($), [I | S1], S2) ->
    pop($), S1, [{'operator', I} | S2]);
pop($(, S1, S2) ->
    {[$( | S1], S2};
pop(Operator, [I | S1], S2) ->
    case pop_priority(Operator, I) of
        true ->%%弹出的元素优先级比O低
            {[Operator, I | S1], S2};
        false ->
            pop(Operator, S1, [{'operator', I} | S2])
    end;
pop(Operator, [], S2) ->
    {[Operator], S2}.
%% ----------------------------------------------------
%% @doc
%%      操作数
%% @end
%% ----------------------------------------------------
operand([], S2) ->
    S2;
operand(Temp, S2) ->
    [{'operand', list_to_integer(lists:reverse(Temp))} | S2].
