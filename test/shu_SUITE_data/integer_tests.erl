-module(integer_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

integer_test_() ->
    Symbols =
        lists:foldl(
          fun shu_symbols:add/2,
          shu_symbols:new(),
          [{file, "INTEGER.SYMBOLS"},
           {file, "SYMBOLS"}]),

    Rules =
        lists:foldl(
          fun shu_rules:add/2,
          shu_rules:new(),
          [{file, "INTEGER.RULES"},
           {file, "RULES"},
           <<"root(X): 无目(X, 整数), eof."/utf8>>]),

    Eval =
        fun (X) ->
                [{term, root, [R]}] = shu_chart:parse(X, {root,1}, Rules, Symbols),
                F = shu_compile:compile(R),
                F()
        end,

    [?_assertEqual(0, Eval("〇")),
     ?_assertEqual(1, Eval("一")),
     ?_assertEqual(2, Eval("二")),
     ?_assertEqual(3, Eval("三")),
     ?_assertEqual(4, Eval("四")),
     ?_assertEqual(5, Eval("五")),
     ?_assertEqual(6, Eval("六")),
     ?_assertEqual(7, Eval("七")),
     ?_assertEqual(8, Eval("八")),
     ?_assertEqual(9, Eval("九")),

     ?_assertEqual(12, Eval("一十二")),
     ?_assertEqual(30, Eval("三十整")),

     ?_assertEqual(456, Eval("四百五十六")),
     ?_assertEqual(780, Eval("七百八十整")),
     ?_assertEqual(901, Eval("九百零一")),
     ?_assertEqual(200, Eval("二百整")),

     ?_assertEqual(3456, Eval("三千四百五十六")),
     ?_assertEqual(7890, Eval("七千八百九十整")),
     ?_assertEqual(1203, Eval("一千二百零三")),
     ?_assertEqual(4500, Eval("四千五百整")),
     ?_assertEqual(6078, Eval("六千零七十八")),
     ?_assertEqual(9010, Eval("九千零一十整")),
     ?_assertEqual(2003, Eval("二千零三")),
     ?_assertEqual(4000, Eval("四千整")),

     ?_assertEqual(12345678, Eval("一千二百三十四万五千六百七十八")),
     ?_assertEqual(9010203, Eval("九百零一万零二百零三")),
     ?_assertEqual(40500006, Eval("四千零五十万零六")),
     ?_assertEqual(7000000, Eval("七百万整")),
     ?_assertEqual(809000, Eval("八十万九千整")),

     ?_assertEqual(100000000, Eval("一亿整")),
     ?_assertEqual(203000000045000, Eval("二百零三万亿零四万五千整")),
     ?_assertEqual(670000008, Eval("六亿七千万零八")),

     ?_assertEqual(900010000000020000000, Eval("九万零一兆零二千万整"))
    ].

integer_array_test_() ->
    Symbols =
        lists:foldl(
          fun shu_symbols:add/2,
          shu_symbols:new(),
          [{file, "INTEGER.SYMBOLS"},
           {file, "SYMBOLS"}]),

    Rules =
        lists:foldl(
          fun shu_rules:add/2,
          shu_rules:new(),
          [{file, "INTEGER.RULES"},
           {file, "RULES"},
           <<"root(X): 无目(X, 数组(整数)), eof."/utf8>>]),

    Eval =
        fun (X) ->
                [{term, root, [R]}] = shu_chart:parse(X, {root,1}, Rules, Symbols),
                F = shu_compile:compile(R),
                F()
        end,

    [?_assertEqual([1,2,3], Eval("一二三")),
     ?_assertEqual([456,78,9], Eval("四百五十六七十八九")),
     ?_assertEqual([10,2], Eval("一十整二"))
    ].

-endif.
