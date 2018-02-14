-module(shu_unify).

-export(
   [unify/3, subst/2, reify/3,
    offset/2, alpha/1]).

lookup(K, []) ->
    {var, K};
lookup(K, [{K,V}|_]) ->
    V;
lookup(K, [_|S]) ->
    lookup(K, S).

lookup_var(V, S) ->
    case lookup(V, S) of
        {var, V1} when V1 =/= V ->
            lookup_var(V1, S);
        Other ->
            Other
    end.

subst({var, V}, S) ->
    case lookup_var(V, S) of
        {var, _} = V1 ->
            V1;
        Other ->
            subst(Other, S)
    end;
subst({term, F, A}, S) when is_atom(F); is_integer(F) ->
    {term, F, subst(A, S)};
subst({tuple, F, A}, S) when is_atom(F); is_integer(F) ->
    {tuple, F,
     [ E || T <- subst(A, S),
            E <- case T of
                     {tuple, F, A1} -> A1;
                     _ -> [T]
                 end ]
    };
subst([], _S) ->
    [];
subst([H|T], S) ->
    [subst(H,S)|subst(T,S)];
subst(X, _) when is_atom(X); is_integer(X) ->
    X.

occurs(X, {var, V}, S) ->
    case lookup_var(V, S) of
        {var, Y} ->
            X =:= Y;
        Other ->
            occurs(X, Other, S)
    end;
occurs(X, {term, F, A}, S) ->
    occurs(X, [F|A], S);
occurs(X, [H|T], S) ->
    occurs(X, H, S) or occurs(X, T, S);
occurs(_, _, _) ->
    false.

unify({var, V1}, Y, S) ->
    case lookup_var(V1, S) of
        {var, X} ->
            case occurs(X, Y, S) of
                true ->
                    case Y of
                        {var, V2} ->
                            case lookup_var(V2, S) of
                                {var, X} ->
                                    S;
                                _ ->
                                    false
                            end;
                        _ ->
                            false
                    end;
                false ->
                    [{X,Y}|S]
            end;
        Other ->
            unify(Other, Y, S)
    end;
unify(X, {var, _} = Y, S) ->
    unify(Y, X, S);
unify({term, F1, A1}, {term, F2, A2}, S) ->
    unify([F1|A1], [F2|A2], S);
unify([], [], S) ->
    S;
unify([H1|T1], [H2|T2], S) ->
    case unify(H1, H2, S) of
    	false ->
    	    false;
    	S1 ->
    	    unify(T1, T2, S1)
    end;
unify(X, X, S) when is_atom(X); is_integer(X) ->
    S;
unify(_, _, _) ->
    false.


reify({var, V}, S, C) ->
    case lookup_var(V, S) of
        {var, N} when is_integer(N) ->
            {{var, N}, S, C};
        {var, V1} ->
            {{var, C}, [{V1, {var, C}}|S], C+1};
        Other ->
            reify(Other, S, C)
    end;
reify({term, F, A}, S, C) ->
    {[F1|A1], S1, C1} = reify([F|A], S, C),
    {{term, F1, A1}, S1, C1};
reify([H|T], S, C) ->
    {H1, S1, C1} = reify(H, S, C),
    {T1, S2, C2} = reify(T, S1, C1),
    {[H1|T1], S2, C2};
reify(X, S, C) ->
    {X, S, C}.


offset({var, N}, C) when is_integer(N) ->
    {var, N+C};
offset({term, F, A}, C) ->
    [F1|A1] = offset([F|A], C),
    {term, F1, A1};
offset({tuple, F, A}, C) ->
    [F1|A1] = offset([F|A], C),
    {tuple, F1, A1};
offset([H|T], C) ->
    [offset(H, C)|offset(T, C)];
offset(X, _) ->
    X.

find(_, []) ->
    error;
find(K, [{K,V}|_]) ->
    {ok, V};
find(K, [_|T]) ->
    find(K,T).

alpha(Term) ->
    {T, _, C} = alpha(Term, [], 0),
    {T, C}.

alpha({var, V}, S, C) ->
    case find(V, S) of
        {ok, N} ->
            {{var, N}, S, C};
        error ->
            {{var, C}, [{V,C}|S], C+1}
    end;
alpha({tuple, F, A}, S, C) ->
    {[F1|A1], S1, C1} = alpha([F|A], S, C),
    {{tuple, F1, A1}, S1, C1};
alpha({term, F, A}, S, C) ->
    {[F1|A1], S1, C1} = alpha([F|A], S, C),
    {{term, F1, A1}, S1, C1};
alpha([H|T], S, C) ->
    {H1, S1, C1} = alpha(H, S, C),
    {T1, S2, C2} = alpha(T, S1, C1),
    {[H1|T1], S2, C2};
alpha(X, S, C) ->
    {X, S, C}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

lookup_test_() ->
    [?_assertEqual({var, x}, lookup(x, [])),
     ?_assertEqual(b, lookup(x, [{y, a}, {x, b}]))].

lookup_var_test_() ->
    [?_assertEqual({var, x}, lookup_var(x, [])),
     ?_assertEqual(b, lookup_var(x, [{x, {var, y}}, {y, b}]))].

subst_test_() ->
    [?_assertEqual({var, x}, subst({var, x}, [])),
     ?_assertEqual(a, subst({var, x}, [{x, a}])),
     ?_assertEqual(b, subst({var, x}, [{x, {var, y}}, {y, b}])),
     ?_assertEqual({term, a, [b]}, subst({term, a, [{var, x}]}, [{x, b}])),
     ?_assertEqual({tuple, a, [b]}, subst({tuple, a, [{var, x}]}, [{x, b}])),
     ?_assertEqual({tuple, a, [b]}, subst({tuple, a, [{tuple, a, [{var, x}]}]}, [{x, b}]))].

occurs_test_() ->
    [?_assertEqual(false, occurs(x, a, [])),
     ?_assertEqual(true, occurs(x, {var, x}, [])),
     ?_assertEqual(false, occurs(y, {var, x}, [])),
     ?_assertEqual(false, occurs(y, {var, x}, [{x, a}])),
     ?_assertEqual(true, occurs(y, {var, x}, [{x, {var, y}}])),
     ?_assertEqual(false, occurs(x, {term, a, []}, [])),
     ?_assertEqual(true, occurs(x, {term, a, [{var, x}]}, [])),
     ?_assertEqual(false, occurs(x, {term, a, [b]}, []))
    ].

unify_test_() ->
    [?_assertEqual(false, unify(a,b,[])),
     ?_assertEqual([], unify(a,a,[])),
     ?_assertEqual(false, unify(1,2,[])),
     ?_assertEqual([], unify(1,1,[])),
     ?_assertEqual([{x,a}], unify({var, x},a,[])),
     ?_assertEqual([{x,a}], unify(a,{var, x},[])),
     ?_assertEqual(false, unify({var, x},b,[{x,a}])),
     ?_assertEqual(false, unify(b,{var, x},[{x,a}])),
     ?_assertEqual([{x,a}], unify({term,a,[{var, x}]},{term,a,[a]},[])),
     ?_assertEqual(false, unify({term,a,[]},{term,b,[]},[])),
     ?_assertEqual([{x,{var, y}}], unify({var, x},{var, y},[])),
     ?_assertEqual([{x,{var, y}}], unify({var, x},{var, y},[{x,{var,y}}])),
     ?_assertEqual([{x,{var, y}}], unify({var, y},{var, x},[{x,{var,y}}])),
     ?_assertEqual(false, unify({var, y},{var, x},[{x,{term,a,[{var,y}]}}])),
     ?_assertEqual(false, unify({var, x},{term, a, [{var, x}]},[]))
    ].

reify_test_() ->
    [?_assertEqual({{var, 0}, [{x, {var, 0}}], 1}, reify({var, x}, [], 0)),
     ?_assertEqual({{term, a, [{var, 0}]}, [{y, {term, a, [{var, x}]}}, {x, {var, 0}}], 1},
                   reify({var, y}, [{y, {term, a, [{var, x}]}}, {x, {var, 0}}], 1))
    ].

offset_test_() ->
    [?_assertEqual({var, 1}, offset({var, 0}, 1)),
     ?_assertEqual({term, a, [{var, 1}]}, offset({term, a, [{var, 0}]}, 1)),
     ?_assertEqual({tuple, a, [{var, 1}]}, offset({tuple, a, [{var, 0}]}, 1))].

alpha_test_() ->
    [?_assertEqual({{var, 0}, 1}, alpha({var, x})),
     ?_assertEqual({{term, a, [{var, 0}, {var, 0}]}, 1}, alpha({term, a, [{var, x}, {var, x}]})),
     ?_assertEqual({{tuple, a, [{var, 0}, {var, 1}, {var, 0}]}, 2}, alpha({tuple, a, [{var, x}, {var, y}, {var, x}]}))].

-endif.
