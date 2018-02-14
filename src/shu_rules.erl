-module(shu_rules).

-export([new/0, add/2]).


new() ->
    #{}.


pred_name(F, A) ->
    {F, length(A)}.

term_to_query(Term) ->
    [F|A] = shu_term:to_list(Term),
    {pred_name(F,A), A}.


add({file, Filename}, Rules) ->
    {ok, Bin} = file:read_file(filename:join(code:priv_dir(shu), Filename)),
    add(Bin, Rules);
add(Bin, Rules) when is_binary(Bin) ->
    lists:foldl(
      fun ({clause, Head, Body}, Map) ->
              [F|A] = shu_term:to_list(Head),
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
      parse(Bin)).


parse(Bin) ->
    Tokens = shu_meta_parse:tokens(Bin),
    shu_meta_parse:rules(Tokens).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    Parse = fun(X) -> parse(unicode:characters_to_binary(X)) end,
    [?_assertEqual([{clause, {term,a,[{var,'X'}]}, [{term,b,[{var,'X'},{ignore,'_'},123]}]}], Parse("a(X) : b(X,_,123).")),
     ?_assertEqual([{clause, {term,a,[{tuple, a, [a,{var,'X'}]}]}, [{term,b,[{var,'X'}]}]}], Parse("a(a{a,X}) : b(X)."))].

-endif.
