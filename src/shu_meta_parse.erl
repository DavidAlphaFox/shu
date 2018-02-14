-module(shu_meta_parse).

-export([tokens/1, rules/1, symbols/1]).


tokens(<<>>) ->
    [];
tokens(<<C, Bin/binary>>)
  when C =:= $ ; C =:= $\n ->
    tokens(Bin);
tokens(<<${, Bin/binary>>) ->
    ['{'|tokens(Bin)];
tokens(<<$}, Bin/binary>>) ->
    ['}'|tokens(Bin)];
tokens(<<$(, Bin/binary>>) ->
    ['('|tokens(Bin)];
tokens(<<$), Bin/binary>>) ->
    [')'|tokens(Bin)];
tokens(<<$,, Bin/binary>>) ->
    [','|tokens(Bin)];
tokens(<<$., Bin/binary>>) ->
    ['.'|tokens(Bin)];
tokens(<<$:, Bin/binary>>) ->
    [':'|tokens(Bin)];
tokens(<<C, Bin/binary>>)
  when $0 =< C, C =< $9 ->
    {I, Bin1} = integer(C - $0, Bin),
    [{integer, I}|tokens(Bin1)];
tokens(<<C, Bin/binary>>)
  when $a =< C, C =< $z ->
    {A, Bin1} = name(C, Bin),
    [{atom, A}|tokens(Bin1)];
tokens(<<C, Bin/binary>>)
  when $A =< C, C =< $Z ->
    {A, Bin1} = name(C, Bin),
    [{var, A}|tokens(Bin1)];
tokens(<<C, Bin/binary>>)
  when $_ =:= C ->
    {A, Bin1} = name(C, Bin),
    [{ignore, A}|tokens(Bin1)];
tokens(<<C/utf8, Bin/binary>>)
  when 128 =< C ->
    {A, Bin1} = literal(C, Bin),
    [{literal, A}|tokens(Bin1)].



name(C, Bin) ->
    {S, Bin1} = name(Bin),
    {list_to_atom([C|S]), Bin1}.

name(<<C, Bin/binary>>)
  when $a =< C, C =< $z;
       $A =< C, C =< $Z;
       $0 =< C, C =< $9;
       C =:= $_ ->
    {S, Bin1} = name(Bin),
    {[C|S], Bin1};
name(Bin) ->
    {[], Bin}.


integer(A, <<C, Bin/binary>>)
  when $0 =< C, C =< $9 ->
    integer(A * 10 + C - $0, Bin);
integer(A, Bin) ->
    {A, Bin}.


literal(C, Bin) ->
    {S, Bin1} = literal(Bin),
    {[C|S], Bin1}.

literal(<<C, Bin/binary>>)
  when $a =< C, C =< $z;
       $A =< C, C =< $Z;
       $0 =< C, C =< $9;
       C =:= $_ ->
    {S, Bin1} = literal(Bin),
    {[C|S], Bin1};
literal(<<C/utf8, Bin/binary>>)
  when 128 =< C ->
    {S, Bin1} = literal(Bin),
    {[C|S], Bin1};
literal(Bin) ->
    {[], Bin}.


rules([]) ->
    [];
rules(List) ->
    {H, List1} = rule(List),
    T = rules(List1),
    [H|T].


rule(List) ->
    {Head, [':'|List1]} = term(List),
    case Head of
        _ when is_atom(Head) ->
            ok;
        _ when is_integer(Head) ->
            ok;
        {term, _, _} ->
            ok
    end,
    {Body, List2} = terms('.', List1),
    {{clause, Head, Body}, List2}.


symbols([]) ->
    [];
symbols([{literal, L}|List]) ->
    {Symbol, List1}= term(List),
    [{L, Symbol}|symbols(List1)].


term([{atom, A}, '{'|List]) ->
    {Terms, List1} = terms('}', List),
    {{tuple, A, Terms}, List1};
term([{atom, A}, '('|List]) ->
    {Terms, List1} = terms(')', List),
    {{term, A, Terms}, List1};
term([{literal, A}, '{'|List]) ->
    {Terms, List1} = terms('}', List),
    {{tuple, list_to_atom(A), Terms}, List1};
term([{literal, A}, '('|List]) ->
    {Terms, List1} = terms(')', List),
    {{term, list_to_atom(A), Terms}, List1};
term([{atom, A}|List]) ->
    {A, List};
term([{literal, A}|List]) ->
    {list_to_atom(A), List};
term([{integer, I}|List]) ->
    {I, List};
term([{var, _} = V|List]) ->
    {V, List};
term([{ignore, _} = I|List]) ->
    {I, List}.

terms(End, List) ->
    {H, List1} = term(List),
    case List1 of
        [','|List2] ->
            {T, List3} = terms(End, List2);
        [End|List3] ->
            T = []
    end,
    {[H|T], List3}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

tokens_test_() ->
    Tokens = fun(X) -> tokens(unicode:characters_to_binary(X)) end,

    [?_assertEqual([], Tokens(" \n\n")),
     ?_assertEqual(['(',')',',','.',':','{','}'], Tokens("(),.:{}")),
     ?_assertEqual([{atom, atom}, {var, 'Var'}, {ignore, '_Ignore'}, {integer, 123}], Tokens("atom Var _Ignore 123"))
    ].

-endif.
