-module(furry_sql_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TESTEE, furry_sql).

simple_select_test() ->
  Query = ?TESTEE:format(#{
    select => [1]
  }),
  ?assertEqual(<<"SELECT 1">>, Query).

from_test() ->
  Query = ?TESTEE:format(#{
    select => [foo, bar],
    from => [table1]
  }),
  ?assertEqual(<<"SELECT foo, bar FROM table1">>, Query).

where_test() ->
  Query = ?TESTEE:format(#{
    select => ['*'],
    from => [tab1],
    where => [eq, id, 17]
  }),
  ?assertEqual(<<"SELECT * FROM tab1 WHERE (id = 17)">>, Query).

complex_where_test() ->
  Query = ?TESTEE:format(#{
    select => ['*'],
    from => [t1],
    where => [
      'and',
      [eq, tel, "17434"],
      [gt, id, 21]
    ]
  }),
  ?assertEqual(<<"SELECT * FROM t1 WHERE ((tel = \"17434\") AND (id > 21))">>, Query).

insert_into_test() ->
  Query = ?TESTEE:format(#{
    insert_into => tab2,
    columns => [foo, bar, baz],
    values => [["Foo", "Bar", 19], ["Test", "Value", 21]]
  }),
  ?assertEqual(<<"INSERT INTO tab2 (foo, bar, baz) VALUES (\"Foo\", \"Bar\", 19), (\"Test\", \"Value\", 21)">>, Query).

delete_from_test() ->
  Query = ?TESTEE:format(#{
    delete_from => tab3,
    where => [lte, id, 100]
  }),
  ?assertEqual(<<"DELETE FROM tab3 WHERE (id <= 100)">>, Query).

update_test() ->
  Query = ?TESTEE:format(#{
    update => tab4,
    set => [
      {foo, "Foo"},
      {bar, 17}
    ],
    where => [eq, baz, 19]
  }),
  ?assertEqual(<<"UPDATE tab4 SET foo = \"Foo\", bar = 17 WHERE (baz = 19)">>, Query).
