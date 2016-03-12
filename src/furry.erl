-module(furry).

%% API exports
-export([format/1]).

%%====================================================================
%% API functions
%%====================================================================

format(SqlMap) ->
  #{select := Select, from := From, where := Where} = SqlMap,
  [Operation, LValue, Binding] = Where,
  FromClause = comp(From, [
    fun map_string/1,
    fun comma_join/1
  ]),
  WhereClause = comp([LValue, operation(Operation), "$1"], [
    fun space_join/1,
    fun paren_wrap/1
  ]),
  Query = space_join([
    "SELECT", comma_join(Select),
    "FROM",   FromClause,
    "WHERE",  WhereClause
  ]),
  [Query, Binding].

%%====================================================================
%% Internal functions
%%====================================================================

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

comp(Initial, Funs) ->
  lists:foldl(fun(Fun, AccIn) ->
    Fun(AccIn)
  end, Initial, Funs).
