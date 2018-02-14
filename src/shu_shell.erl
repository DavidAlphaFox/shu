-module(shu_shell).

-export([start/0, server/0]).

start() ->
    spawn(fun () -> server() end).

server() ->
    process_flag(trap_exit, true),

    Symbols =
        lists:foldl(
          fun shu_parser:add_symbols/2,
          #{},
          [{file, "INTEGER.SYMBOLS"},
           {file, "SYMBOLS"}]),

    Rules =
        lists:foldl(
          fun shu_parser:add_rules/2,
          #{},
          [{file, "INTEGER.RULES"},
           {file, "RULES"}]),

    server_loop(Rules, Symbols).

server_loop(Rules, Symbols) ->
    Prompt = prompt(),
    Line = io:get_line(Prompt),

    case shu_parser:parse(
           string:strip(Line, both, $\n),
           {root,2},
           Rules,
           Symbols)
    of
        [{term, root, [R,T]}] ->
            io:format("~ts ~tp~n", [shu_parser:format(R),T]);
        [] ->
            io:format("~ts~n", ["错误"]);
        Choices ->
            io:format("~ts~n", ["歧义"]),
            [ io:format("  ~ts ~tp~n", [shu_parser:format(R),T])
              || {term, root, [R,T]} <- Choices]
    end,
    server_loop(Rules, Symbols).


prompt() ->
    "问曰".
