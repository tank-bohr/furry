-module(furry).

%% API exports
-export([format/1]).

%%====================================================================
%% API functions
%%====================================================================

format(SqlMap) ->
  format(SqlMap, undefined, []).

%%====================================================================
%% Internal functions
%%====================================================================

format(#{select := Select} = SqlMap, _, Bindings) ->
  Sql = space_join(["SELECT", select_clause(Select)]),
  format(maps:remove(select, SqlMap), Sql, Bindings);
format(#{from := From} = SqlMap, Sql0, Bindings) ->
  Sql = space_join([Sql0, "FROM", from_clause(From)]),
  format(maps:remove(from, SqlMap), Sql, Bindings);
format(#{where := Where} = SqlMap, Sql0, Bindings) ->
  Sql = space_join([Sql0, "WHERE", where_clause(Where)]),
  format(maps:remove(where, SqlMap), Sql, Bindings);
format(#{}, Sql, Bindings) ->
  [Sql | Bindings].

select_clause(Select) ->
  comma_join(Select).

from_clause(From) ->
  comma_join(lists:map(fun handle_from/1, From)).

where_clause(Where) ->
  handle_operation(Where).

handle_from({TableName, Alias}) ->
  space_join([
    to_string(TableName),
    "AS",
    quotes_wrap(Alias)
  ]);
handle_from(TableName) ->
  to_string(TableName).

handle_operation([Operation | Args]) when is_atom(Operation) ->
  paren_wrap(string:join(
    lists:map(fun handle_operation/1, Args),
    space_wrap(operation(Operation))
  ));
handle_operation(Arg) -> Arg.

operation(eq) -> "=";
operation(ne) -> "<>";
operation(gt) -> ">";
operation(lt) -> "<";
operation(ge) -> ">=";
operation(le) -> "<=";
operation(is) -> "IS";
operation(is_not) -> "IS NOT".

to_string(Bin)  when is_binary(Bin) -> binary_to_list(Bin);
to_string(Atom) when is_atom(Atom)  -> atom_to_list(Atom);
to_string(List) when is_list(List)  -> List.

map_string(L) -> lists:map(fun to_string/1, L).

comma_join(S) -> string:join(map_string(S), ", ").

space_join(S) -> string:join(map_string(S), " ").

parameterize() -> not_implemented.

paren_wrap(X) ->
  comp(["(", X, ")"], [
    fun erlang:iolist_to_binary/1,
    fun to_string/1
  ]).

quotes_wrap(X) ->
  comp(["\"", X, "\""], [
    fun erlang:iolist_to_binary/1,
    fun to_string/1
  ]).

space_wrap(X) ->
  comp([" ", X, " "], [
    fun erlang:iolist_to_binary/1,
    fun to_string/1
  ]).

comp(Initial, Funs) ->
  lists:foldl(fun(Fun, AccIn) ->
    Fun(AccIn)
  end, Initial, Funs).
