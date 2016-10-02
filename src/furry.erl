-module(furry).

-export([format/1]).

format(Map) ->
  format(Map, undefined).

format(#{select := SelectClause} = Map, _) ->
  Acc = [
    <<"SELECT ">>,
    join_with_comma(quote_args(SelectClause))
  ],
  format(maps:remove(select, Map), Acc);
format(#{from := FromClause} = Map, Acc0) ->
  Acc = [
    Acc0, <<" FROM ">>,
    join_with_comma(quote_args(FromClause))
  ],
  format(maps:remove(from, Map), Acc);
format(#{where := WhereClause} = Map, Acc0) ->
  Acc = [
    Acc0,
    <<" WHERE ">>,
    build_conditions(WhereClause)
  ],
  format(maps:remove(where, Map), Acc);
format(#{}, Acc) ->
  iolist_to_binary(Acc).

quote_args(Args) ->
  lists:map(fun quote/1, Args).

quote(Atom) when is_atom(Atom) ->
  atom_to_binary(Atom, utf8);
quote(Int) when is_number(Int) ->
  integer_to_binary(Int);
quote(List) when is_list(List) ->
  wrap_with_quots(List).

join_with_comma(List) ->
  join_binareis(List, ", ").

join_binareis([], _Sep) ->
  [];
join_binareis([H|T], Sep) ->
  iolist_to_binary([H, [[Sep, X] || X <- T]]).

wrap_with_quots(Str) ->
  iolist_to_binary(["\"", Str, "\""]).

wrap_with_parentheses(Str) ->
  iolist_to_binary(["(", Str, ")"]).

build_conditions(['and'|Conditions]) ->
  ConditionBinaries = [
    wrap_with_parentheses(build_conditions(C)) ||
    C <- Conditions
  ],
  join_binareis(ConditionBinaries, " AND ");
build_conditions([eq|Args]) ->
  join_binareis(quote_args(Args), " = ");
build_conditions([gt|Args]) ->
  join_binareis(quote_args(Args), " > ").
