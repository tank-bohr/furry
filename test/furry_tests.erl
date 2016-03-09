-module(furry_tests).
-include_lib("eunit/include/eunit.hrl").

format_test() ->
  SqlMap = #{
    select => [a, b, c],
    from => [foo],
    where => [eq, "f.a", "baz"]
  },
  Query = furry:format(SqlMap),
  ?assertEqual([<<"SELECT a, b, c FROM foo WHERE f.a = $1">>, "baz"], Query).
