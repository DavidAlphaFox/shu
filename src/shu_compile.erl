-module(shu_compile).

-export([compile/1]).

compile({tuple, '并列', A}) ->
    Funs = [compile(T) || T <- A ],
    fun (X) ->
            [F(X) || F <- Funs]
    end;
compile({term, '顺序', [A,B]}) ->
    A1 = compile(A),
    B1 = compile(B),
    fun(X) ->
            B1(A1(X))
    end;
compile({term, '组合', [A,B]}) ->
    A1 = compile(A),
    B1 = compile(B),
    A1(B1);
compile({term, '组合', [A,B,C]}) ->
    A1 = compile(A),
    B1 = compile(B),
    C1 = compile(C),
    fun(X) ->
            A1(B1(X),C1(X))
    end;
compile({term, '整数', [X]}) ->
    Int = '整数'(X),
    fun (_) -> Int end;
compile({term, '左目', ['以']}) ->
    fun(F) -> fun (X,Y) -> F(Y,X) end end;
compile({term, '左目', ['项数']}) ->
    fun length/1;
compile({term, '右目', ['相']}) ->
    fun (F) ->
        fun([H|T]) ->
                lists:foldl(F,H,T)
        end
    end;
compile({term, '双目', ['加']}) ->
    fun erlang:'+'/2;
compile({term, '双目', ['除']}) ->
    fun (X, Y) -> Y div X end.


'整数'({term, '乘', [A,B]}) ->
    '整数'(A) * '整数'(B);
'整数'({term, '加', [A,B]}) ->
    '整数'(A) + '整数'(B);
'整数'(X) when is_integer(X) ->
    X.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

integer_test_() ->
    Symbols =
        lists:foldl(
          fun shu_symbols:add/2,
          shu_symbols:new(),
          [{file, "INTEGER.SYMBOLS"}]),

    Rules =
        lists:foldl(
          fun shu_rules:add/2,
          shu_rules:new(),
          [{file, "INTEGER.RULES"},
           <<"root(X): 整数(X, true), eof."/utf8>>]),

    Parse =
        fun (X) ->
                [{term, root, [R]}] = shu_chart:parse(X, {root,1}, Rules, Symbols),
                '整数'(R)
        end,

    [?_assertEqual(0, Parse("〇")),
     ?_assertEqual(1, Parse("一")),
     ?_assertEqual(2, Parse("二")),
     ?_assertEqual(3, Parse("三")),
     ?_assertEqual(4, Parse("四")),
     ?_assertEqual(5, Parse("五")),
     ?_assertEqual(6, Parse("六")),
     ?_assertEqual(7, Parse("七")),
     ?_assertEqual(8, Parse("八")),
     ?_assertEqual(9, Parse("九")),

     ?_assertEqual(12, Parse("一十二")),
     ?_assertEqual(30, Parse("三十整")),

     ?_assertEqual(456, Parse("四百五十六")),
     ?_assertEqual(780, Parse("七百八十整")),
     ?_assertEqual(901, Parse("九百零一")),
     ?_assertEqual(200, Parse("二百整")),

     ?_assertEqual(3456, Parse("三千四百五十六")),
     ?_assertEqual(7890, Parse("七千八百九十整")),
     ?_assertEqual(1203, Parse("一千二百零三")),
     ?_assertEqual(4500, Parse("四千五百整")),
     ?_assertEqual(6078, Parse("六千零七十八")),
     ?_assertEqual(9010, Parse("九千零一十整")),
     ?_assertEqual(2003, Parse("二千零三")),
     ?_assertEqual(4000, Parse("四千整")),

     ?_assertEqual(12345678, Parse("一千二百三十四万五千六百七十八")),
     ?_assertEqual(9010203, Parse("九百零一万零二百零三")),
     ?_assertEqual(40500006, Parse("四千零五十万零六")),
     ?_assertEqual(7000000, Parse("七百万整")),
     ?_assertEqual(809000, Parse("八十万九千整")),

     ?_assertEqual(100000000, Parse("一亿整")),
     ?_assertEqual(203000000045000, Parse("二百零三万亿零四万五千整")),
     ?_assertEqual(670000008, Parse("六亿七千万零八")),

     ?_assertEqual(900010000000020000000, Parse("九万零一兆零二千万整"))
].

-endif.
