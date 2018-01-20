-module(shu_symbol_parser).

-export([parse/1]).

parse(Bin) ->
    Tokens = tokens(Bin),
    symbols(Tokens).

tokens(<<>>) ->
    [];
tokens(<<C, Bin/binary>>)
  when C =:= $ ; C =:= $\n ->
    tokens(Bin);
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
    io:format("~p~n", [C]),
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


literal(C, Bin) ->
    {S, Bin1} = literal(Bin),
    {[C|S], Bin1}.

literal(<<C/utf8, Bin/binary>>)
  when 128 =< C ->
    {S, Bin1} = literal(Bin),
    {[C|S], Bin1};
literal(Bin) ->
    {[], Bin}.


symbols([]) ->
    [];
symbols([{literal, L}|List]) ->
    {{term, symbol, Symbol}, List1}= term(List),
    [{L, Symbol}|symbols(List1)].


term([{atom, A}, '('|List]) ->
    {Terms, List1} = terms(')', List),
    {{term, A, Terms}, List1};
term([{atom, A}|List]) ->
    {A, List};
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
     ?_assertEqual([{literal, "多少"}], Tokens("多少")),
     ?_assertEqual(['(',')',',','.',':'], Tokens("(),.:")),
     ?_assertEqual([{atom, atom}, {var, 'Var'}, {ignore, '_Ignore'}], Tokens("atom Var _Ignore"))
    ].

parse_test_() ->
    Parse = fun(X) -> parse(unicode:characters_to_binary(X)) end,
    [?_assertEqual([{"多少", [x, {term, t, [{var, 'V'}, {ignore, '_'}]}]}], Parse("多少 symbol(x, t(V,_))"))].

-endif.
