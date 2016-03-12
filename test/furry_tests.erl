-module(furry_tests).
-include_lib("eunit/include/eunit.hrl").

format_test() ->
  SqlMap = #{
    select => [a, b, c],
    from => [foo],
    where => [eq, "f.a", "baz"]
  },
  Query = furry:format(SqlMap),
  ?assertEqual(["SELECT a, b, c FROM foo WHERE (f.a = $1)", "baz"], Query).

format_big_query_test() ->
  SqlMap = #{
    select => ["f.*", "b.baz", "c.quux", {"b.bla", "bla-bla"}],
    modifiers => [distinct],
    from => [{foo, f}, {baz, b}],
    join => [draq, [eq, "f.b" "draq.x"]],
    left_join => [{clod, c}, [eq, "f.a", "c.d"]],
    right_join => [bock, [eq, "bock.z" "c.e"]],
    where => ['or',
      ['and', [eq, "f.a", "bort"], [ne, "b.baz", "param1"]],
      [lt, 1, 2, 3],
      [in, "f.e", [1, 2, 3]],
      [between, "f.e", 10, 20]
    ],
    group_by => ["f.a"],
    having => [le, [0, "f.e"]],
    order_by => [{"b.baz", desc}, "c.quux", {"f.a", nulls_first}],
    limit => 50,
    offset => 10
  },
  QueryExpected = "SELECT DISTINCT f.*, b.baz, c.quux, b.bla AS \"bla-bla\" FROM foo AS f, baz AS b INNER JOIN draq ON f.b = draq.x LEFT JOIN clod AS c ON f.a = c.d RIGHT JOIN bock ON bock.z = c.e WHERE ((f.a = ? AND b.baz <> ?) OR (1 < 2 AND 2 < 3) OR (f.e IN (1, ?, 3)) OR f.e BETWEEN 10 AND 20) GROUP BY f.a HAVING 0 < f.e ORDER BY b.baz DESC, c.quux, f.a NULLS FIRST LIMIT 50 OFFSET 10",
  ?assertEqual([QueryExpected | ["bort", "gabba", 2]], furry:format(SqlMap)).
