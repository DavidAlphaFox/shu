-module(integer_tests).

eval({term, '乘', [A,B]}) ->
    eval(A) * eval(B);
eval({term, '加', [A,B]}) ->
    eval(A) + eval(B);
eval(X) when is_integer(X) ->
    X.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

integer_test_() ->
    Symbols = shu_parser:symbols("INTEGER.SYMBOLS"),
    Rules = shu_parser:rules("INTEGER.RULES"),
    Parse =
        fun (X) ->
                [{term, root, [R]}] = shu_parser:parse(X, {root,1}, Rules, Symbols),
                eval(R)
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
