-module(shu_chart).

-export([parse/4]).


new(Root, Grammar) ->
    States = #{0 => predict(0, Root, #{}, Grammar)},
    #{root => Root,
      grammar => Grammar,
      states => States,
      results => #{}}.


add_state(Chart = #{grammar := Grammar, states := States}, I, Name, State) ->
    States1 = States#{I => add_state(I, Name, State, maps:get(I, States, #{}), Grammar)},
    Chart#{states := States1}.


add_result(Chart = #{results := Results}, I, Name, Result) ->
    Map = maps:get(I, Results, #{}),
    List = maps:get(Name, Map, []),

    case lists:member(Result, List)  of
        true ->
            {existed, Chart};
        false ->
            Results1 = Results#{I => Map#{Name => lists:append(List, [Result])}},
            {ok, Chart#{results := Results1}}
    end.


add_state(I, Name, State, States, Grammar) ->
    States1 =
        maps:put(
          Name,
          lists:append(maps:get(Name, States, []), [State]),
          States),

    case maps:is_key(Name, States) of
        true ->
            States1;
        false ->
            predict(I, Name, States1, Grammar)
    end.


predict(I, Name, States, Grammar) ->
    lists:foldl(
      fun({rule, Head, [{F,A}|Body]}, S) ->
              add_state(I, F, {A, Body, [], 0, I, Head}, S, Grammar)
      end,
      States,
      case maps:find(Name, Grammar) of
          error -> [];
          {ok, Rules} -> Rules
      end).


complete(Chart = #{states := States}, I, Name, N, {Entry, Count}) ->
    lists:foldl(
      fun ({A, Body, S, Next, I1, Head = {F1,A1}}, C) ->
              Next1 = Next + Count,
              case shu_unify:unify(A, shu_unify:offset(Entry, Next), S) of
                  false ->
                      C;
                  S1 ->
                      case Body of
                          [] ->
                              A2 = shu_unify:alpha(shu_unify:subst(A1, S1)),
                              case add_result(C, I1, F1, A2) of
                                  {ok, C1} ->
                                      complete(C1, I, F1, I1, A2);
                                  {existed, C1} ->
                                      C1
                              end;
                          [{F2,A2}|Body1] ->
                              add_state(C, I+1, F2, {A2, Body1, S1, Next1, I1, Head})
                      end
              end
      end,
      Chart,
      maps:get(Name, maps:get(N, States, #{}), [])).


parse(I, [], Chart, _Tables, _Symbols) ->
    complete(Chart, I, {eof, 0}, I, {[], 0});
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
                  complete(C, I, Name, N, Entry)
          end,
          Chart,
          Entries),

    Tables3 =
        [ {N,Table}
          || {N,Table} <- Tables2,
             maps:size(Table) > 0],

    parse(I+1, T, Chart1, Tables3, Symbols).

parse(List, {F,_} = Root, Grammar, Symbols) ->
    #{results := Results} = parse(0, List, new(Root, Grammar), [], Symbols),
    [ {term, F, Term} || {Term, _} <- maps:get(Root, maps:get(0, Results, #{}), [])].
