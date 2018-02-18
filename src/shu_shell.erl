-module(shu_shell).

-export([start/0, server/0]).

start() ->
    spawn(fun () -> server() end).

server() ->
    process_flag(trap_exit, true),

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
           <<"root(X, T): 顺序(X, T), eof."/utf8>>]),

    server_loop(Rules, Symbols).

server_loop(Rules, Symbols) ->
    Prompt = prompt(),
    Line = io:get_line(Prompt),

    case shu_chart:parse(
           string:strip(Line, right, $\n),
           {root,2},
           Rules,
           Symbols)
    of
        [{term, root, [R,_]}] ->
            F = shu_compile:compile(R),
            io:format("~ts~p~n", ["答曰", F()]);
        [] ->
            io:format("~ts~n", ["错误"]);
        Choices ->
            io:format("~ts~n", ["歧义"]),
            [ io:format("  ~ts ~ts~n", [shu_term:format(R),shu_term:format(T)])
              || {term, root, [R,T]} <- Choices]
    end,
    server_loop(Rules, Symbols).


prompt() ->
    "问曰".
