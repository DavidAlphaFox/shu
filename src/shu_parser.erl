-module(shu_parser).

-export(
   [add_rules/2,
    add_symbols/2,
    parse/4,
    format/1]).


term_to_list({term, F, A}) ->
    [F|A];
term_to_list(Atom) when is_atom(Atom) ->
    [Atom];
term_to_list(Int) when is_integer(Int) ->
    [Int].

pred_name(F, A) ->
    {F, length(A)}.

term_to_query(Term) ->
    [F|A] = term_to_list(Term),
    {pred_name(F,A), A}.


add_rules({file, Filename}, Rules) ->
    {ok, Bin} = file:read_file(filename:join(code:priv_dir(shu), Filename)),
    add_rules(Bin, Rules);
add_rules(Bin, Rules) ->
    lists:foldl(
      fun ({clause, Head, Body}, Map) ->
              [F|A] = term_to_list(Head),
              Key = pred_name(F,A),
              Value =
                  lists:append(
                    maps:get(Key, Map, []),
                    [{rule, term_to_query(Head),
                      [term_to_query(T) || T <- Body]}]
                   ),
              maps:put(Key, Value, Map)
      end,
      Rules,
      shu_rule_parser:parse_rules(Bin)).


add_symbols({file, Filename}, Symbols) ->
    {ok, Bin} = file:read_file(filename:join(code:priv_dir(shu), Filename)),
    add_symbols(Bin, Symbols);
add_symbols(Bin, Symbols) ->
    lists:foldl(
      fun ({K,V}, Trie) ->
              {T, C} = shu_unify:alpha(V),
              [F|A] = term_to_list(T),
              shu_trie:add(K, {{F, length(A)}, {A, C}}, Trie)
      end,
      Symbols,
      shu_rule_parser:parse_symbols(Bin)).


parse(I, [], Chart, _Tables, _Symbols) ->
    shu_chart:complete(Chart, I, {eof, 0}, I, {[], 0});
parse(I, [H|T], Chart, Tables, Symbols) ->
    Tables1 = [{I, Symbols}|Tables],

    Tables2 =
        [ {N,maps:get(H,Table)}
          || {N,Table} <- Tables1,
             maps:is_key(H, Table)],

    Entries =
        [ {N, Entry}
          || {N,Table} <- Tables2,
             Entry <- maps:get([],Table,[])],

    Chart1 =
        lists:foldl(
          fun ({N, {Name,Entry}}, C) ->
                  shu_chart:complete(C, I, Name, N, Entry)
          end,
          Chart,
          Entries),

    Tables3 =
        [ {N,Table}
          || {N,Table} <- Tables2,
             maps:size(Table) > 0],

    parse(I+1, T, Chart1, Tables3, Symbols).

parse(List, {F,_} = Root, Grammar, Symbols) ->
    #{results := Results} = parse(0, List, shu_chart:new(Root, Grammar), [], Symbols),
    [ {term, F, Term} || {Term, _} <- maps:get(Root, maps:get(0, Results, #{}), [])].


format({tuple, Name, List}) ->
    io_lib:format("~s{~s}", [Name, format_list(List)]);
format({term, Name, List}) ->
    io_lib:format("~s(~s)", [Name, format_list(List)]);
format({var, Name}) ->
    io_lib:format("~s", [Name]);
format(Atom) when is_atom(Atom) ->
    io_lib:format("~s", [Atom]).

format_list(List) ->
    string:join([format(T) || T <- List], ",").
