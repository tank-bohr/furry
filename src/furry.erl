-module(furry).

%% API exports
-export([format/1]).

%%====================================================================
%% API functions
%%====================================================================

format(SqlMap) ->
  #{select := Select, from := From, where := Where} = SqlMap,
  SelectStr = string:join(lists:map(fun to_string/1, Select), ", "),
  FromStr = lists:map(fun to_string/1, From),
  [Operation, LValue, Binding] = Where,
  OperationStr = operation(Operation),
  Query = iolist_to_binary([
    "SELECT ", SelectStr,
    " FROM ", FromStr,
    " WHERE ", LValue, OperationStr, "$1"]),
  [Query, Binding].

%%====================================================================
%% Internal functions
%%====================================================================

operation(eq) -> " = ".

to_string(Bin) when is_binary(Bin) ->
  binary_to_list(Bin);
to_string(Atom) when is_atom(Atom) ->
  atom_to_list(Atom);
to_string(List) when is_list(List) ->
  List.
