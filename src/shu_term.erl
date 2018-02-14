-module(shu_term).

-export(
   [ to_list/1,
     format/1 ]).


to_list({term, F, A}) ->
    [F|A];
to_list(Atom) when is_atom(Atom) ->
    [Atom];
to_list(Int) when is_integer(Int) ->
    [Int].


format({tuple, Name, List}) ->
    io_lib:format("~ts{~ts}", [Name, format_list(List)]);
format({term, Name, List}) ->
    io_lib:format("~ts(~ts)", [Name, format_list(List)]);
format({var, Name}) ->
    io_lib:format("~ts", [Name]);
format(Atom) when is_atom(Atom) ->
    io_lib:format("~ts", [Atom]);
format(Int) when is_integer(Int) ->
    io_lib:format("~B", [Int]).


format_list(List) ->
    string:join([format(T) || T <- List], ",").
