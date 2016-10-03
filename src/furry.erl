-module(furry).

-export([format/1]).

format(Map) ->
  format(Map, undefined).

format(#{select := SelectClause} = Map, _) ->
  format(maps:remove(select, Map), [
    <<"SELECT ">>, quote_and_join(SelectClause, ", ")
  ]);
format(#{from := FromClause} = Map, Acc) ->
  format(maps:remove(from, Map), [
    Acc, <<" FROM ">>, quote_and_join(FromClause, ", ")
  ]);
format(#{where := WhereClause} = Map, Acc) ->
  format(maps:remove(where, Map), [
    Acc, <<" WHERE ">>, build_conditions(WhereClause)
  ]);
format(#{insert_into := Table} = Map, _) ->
  format(maps:remove(insert_into, Map), [
    <<"INSERT INTO ">>, quote(Table)
  ]);
format(#{columns := Columns} = Map, Acc) ->
  format(maps:remove(columns, Map), [
    Acc, " ", quote_and_join_and_wrap(Columns, ", ")
  ]);
format(#{values := Values} = Map, Acc) ->
  format(maps:remove(values, Map), [
    Acc, " VALUES ", join(
      [quote_and_join_and_wrap(V, ", ") || V <- Values], ", ")
  ]);
format(#{}, Acc) ->
  iolist_to_binary(Acc).

quote_args(Args) ->
  lists:map(fun quote/1, Args).

quote(Atom) when is_atom(Atom) ->
  atom_to_binary(Atom, utf8);
quote(Int) when is_number(Int) ->
  integer_to_binary(Int);
quote(List) when is_list(List) ->
  wrap_with_quots(List);
quote(Bin) when is_binary(Bin) ->
  Bin.

quote_and_join(Args, Sep) ->
  comp(Args, [
    fun quote_args/1,
    curry(fun join/2, [Sep])
  ]).

quote_and_join_and_wrap(Args, Operation) ->
  comp(Args, [
    curry(fun quote_and_join/2, [Operation]),
    fun wrap_with_parentheses/1
  ]).

join([], _Sep) ->
  [];
join([H|T], Sep) ->
  iolist_to_binary([H, [[Sep, X] || X <- T]]).

wrap_with_quots(Str) ->
  iolist_to_binary(["\"", Str, "\""]).

wrap_with_parentheses(Str) ->
  iolist_to_binary(["(", Str, ")"]).

build_conditions(['and'|Conditions]) ->
  Args = [build_conditions(C) || C <- Conditions],
  quote_and_join_and_wrap(Args, " AND ");
build_conditions(['or'|Conditions]) ->
  Args = [build_conditions(C) || C <- Conditions],
  quote_and_join_and_wrap(Args, " OR ");
build_conditions([eq|Args]) ->
  quote_and_join_and_wrap(Args, " = ");
build_conditions([ne|Args]) ->
  quote_and_join_and_wrap(Args, " != ");
build_conditions([lt|Args]) ->
  quote_and_join_and_wrap(Args, " < ");
build_conditions([gt|Args]) ->
  quote_and_join_and_wrap(Args, " > ");
build_conditions([lte|Args]) ->
  quote_and_join_and_wrap(Args, " <= ");
build_conditions([gte|Args]) ->
  quote_and_join_and_wrap(Args, " >= ").

comp(Init, Funs) ->
  lists:foldl(fun(Fun, AccIn) -> Fun(AccIn) end, Init, Funs).

curry(Fun, Args) ->
  fun(FirstArg) -> apply(Fun, [FirstArg|Args]) end.
