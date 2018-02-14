-module(shu_trie).

-export([add/3]).


add([], Value, Trie) ->
    Trie#{[] => lists:append(maps:get([], Trie, []), [Value])};
add([H|T], Value, Trie) ->
    Trie#{H => add(T, Value, maps:get(H, Trie, #{}))}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
    ?assertEqual(#{$多 => #{$少 => #{[] => [1]}}}, add("多少", 1, #{})).

-endif.
