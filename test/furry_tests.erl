-module(furry_tests).
-include_lib("eunit/include/eunit.hrl").

simple_select_test() ->
  Query = furry:format(#{
    select => [1]
  }),
  ?assertEqual(<<"SELECT 1">>, Query).

from_test() ->
  Query = furry:format(#{
    select => [foo, bar],
    from => [table1]
  }),
  ?assertEqual(<<"SELECT foo, bar FROM table1">>, Query).

where_test() ->
  Query = furry:format(#{
    select => ['*'],
    from => [tab1],
    where => [eq, id, 17]
  }),
  ?assertEqual(<<"SELECT * FROM tab1 WHERE id = 17">>, Query).

complex_where_test() ->
  Query = furry:format(#{
    select => ['*'],
    from => [t1],
    where => [
      'and',
      [eq, tel, "17434"],
      [gt, id, 21]
    ]
  }),
  ?assertEqual(<<"SELECT * FROM t1 WHERE (tel = \"17434\") AND (id > 21)">>, Query).
