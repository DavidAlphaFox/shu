-module(rules_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


'rules_test_'() ->
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
           <<"root(X): 顺序(X, T), eof."/utf8>>]),

    Eval =
        fun (X) ->
                [{term, root, [R]}] = shu_chart:parse(X, {root,1}, Rules, Symbols),
                F = shu_compile:compile(R),
                F()
        end,

    [%% 连接无目
     ?_assertEqual(5, Eval("一乘二加三")),
     ?_assertEqual(9, Eval("一加二乘三")),
     %% 并列无目
     ?_assertEqual([3,12], Eval("一加二三乘四")),


     %% 顺序
     ?_assertEqual(3, Eval("一二三项数")),

     %% 组合
     ?_assertEqual(6, Eval("一二三相加")),
     ?_assertEqual(2, Eval("一二三相加除以项数")),

     %% 无目转左目
     ?_assertEqual(4, Eval("一二三项数加一")),

     %% 连接
     ?_assertEqual(0, Eval("一二三相乘减相加")),
     ?_assertEqual(0, Eval("一二三相乘减相加除以项数"))
    ].

-endif.
