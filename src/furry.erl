-module(furry).

-export([format/1]).

format(Map) ->
  format(Map, undefined).

format(#{select := SelectClause} = Map, _) ->
  List = lists:map(fun to_binary/1, SelectClause),
  Acc = [<<"SELECT ">>, join_with_comma(List)],
  format(maps:remove(select, Map), Acc);
format(#{from := FromClause} = Map, Acc0) ->
  List = lists:map(fun to_binary/1, FromClause),
  Acc = [Acc0, <<" FROM ">>, join_with_comma(List)],
  format(maps:remove(from, Map), Acc);
format(#{where := WhereClause} = Map, Acc0) ->
  Where = build_conditions(WhereClause),
  Acc = [Acc0, <<" WHERE ">>, Where],
  format(maps:remove(where, Map), Acc);
format(#{}, Acc) ->
  iolist_to_binary(Acc).

to_binary(Atom) when is_atom(Atom) ->
  atom_to_binary(Atom, utf8);
to_binary(Int) when is_number(Int) ->
  integer_to_binary(Int);
to_binary(List) when is_list(List) ->
  iolist_to_binary(List);
to_binary(Bin) when is_binary(Bin) ->
  Bin;
to_binary(_) ->
  <<>>.

join_with_comma(List) ->
  join_binareis(List, ", ").

join_binareis([], _Sep) ->
  [];
join_binareis([H|T], Sep) ->
  iolist_to_binary([H, [[Sep, X] || X <- T]]).

wrap_with_parentheses(Bin) ->
  iolist_to_binary(["(", Bin, ")"]).

build_conditions(['and'|Conditions]) ->
  ConditionBinaries = [
    wrap_with_parentheses(build_conditions(C)) ||
    C <- Conditions
  ],
  join_binareis(ConditionBinaries, " AND ");
build_conditions([eq|Args]) ->
  Binaries = [to_binary(X) || X <- Args],
  join_binareis(Binaries, " = ");
build_conditions([gt|Args]) ->
  Binaries = [to_binary(X) || X <- Args],
  join_binareis(Binaries, " > ").
