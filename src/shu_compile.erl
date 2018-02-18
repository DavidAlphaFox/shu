-module(shu_compile).

-export([compile/1]).


compile({term, '双目', ['加']}) ->
    fun erlang:'+'/2;
compile({term, '双目', ['减']}) ->
    fun erlang:'-'/2;
compile({term, '双目', ['乘']}) ->
    fun erlang:'*'/2;
compile({term, '双目', ['除']}) ->
    fun (X, Y) -> Y div X end;

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

compile({term, '整数', [X]}) ->
    Int = '整数'(X),
    fun () -> Int end;

compile({tuple, '并列无目', L}) ->
    Funs = [compile(T) || T <- L ],
    fun () ->
            [F() || F <- Funs]
    end;
compile({tuple, '并列左目', L}) ->
    Funs = [compile(T) || T <- L ],
    fun (X) ->
            [F(X) || F <- Funs]
    end;
compile({tuple, '连接无目',L}) ->
    Funs = [compile(T) || T <- L ],
    (fun F([E]) ->
             E;
         F([A,B,C|T]) ->
             F([fun() -> B(A(), C()) end|T])
     end)(Funs);

compile({tuple, '连接左目', L}) ->
    Funs = [compile(T) || T <- L ],
    (fun F([E]) ->
             E;
         F([A,B,C|T]) ->
             F([fun(X) -> B(A(X), C(X)) end|T])
     end)(Funs);

compile({term, '组合无目', [A,B,C]}) ->
    A1 = compile(A),
    B1 = compile(B),
    C1 = compile(C),
    fun() ->
            A1(B1(),C1())
    end;
compile({term, '组合左目', [A,B,C]}) ->
    A1 = compile(A),
    B1 = compile(B),
    C1 = compile(C),
    fun(X) ->
            A1(B1(X),C1(X))
    end;

compile({term, '组合', [A,B]}) ->
    A1 = compile(A),
    B1 = compile(B),
    A1(B1);

compile({term, '无目转左目', [A]}) ->
    A1 = compile(A),
    fun(_) ->
            A1()
    end;

compile({term, '顺序无目', [A,B]}) ->
    A1 = compile(A),
    B1 = compile(B),
    fun() ->
            B1(A1())
    end;

compile({term, '顺序左目', [A,B]}) ->
    A1 = compile(A),
    B1 = compile(B),
    fun(X) ->
            B1(A1(X))
    end.


'整数'({term, '乘', [A,B]}) ->
    '整数'(A) * '整数'(B);
'整数'({term, '加', [A,B]}) ->
    '整数'(A) + '整数'(B);
'整数'(X) when is_integer(X) ->
    X.
