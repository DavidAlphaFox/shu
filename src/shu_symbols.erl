-module(shu_symbols).

-export([new/0, add/2]).


new() ->
    #{}.


add({file, Filename}, Symbols) ->
    {ok, Bin} = file:read_file(filename:join(code:priv_dir(shu), Filename)),
    add(Bin, Symbols);
add(Bin, Symbols) when is_binary(Bin) ->
    lists:foldl(
      fun ({K,V}, Trie) ->
              {T, C} = shu_unify:alpha(V),
              [F|A] = shu_term:to_list(T),
              shu_trie:add(K, {{F, length(A)}, {A, C}}, Trie)
      end,
      Symbols,
      parse(Bin)).


parse(Bin) ->
    Tokens = shu_meta_parse:tokens(Bin),
    shu_meta_parse:symbols(Tokens).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    Parse = fun(X) -> parse(unicode:characters_to_binary(X)) end,
    [?_assertEqual([{"多少", {term, symbol, [x, {term, t, [{var, 'V'}, {ignore, '_'}]}, 123]}}], Parse("多少 symbol(x, t(V,_), 123)"))].

-endif.
