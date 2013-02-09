-module(macro_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("feta/include/macro.hrl").

-record(abc, {a, b, c}).

rec_to_kv_test() ->
   [{a, 1}, {b, "2"}, {c, <<"3">>}] = ?rec_to_kv(abc, #abc{a = 1, b = "2", c = <<"3">>}).

kv_to_rec_test() ->
   #abc{a = 1, b = "2", c = <<"3">>} = ?kv_to_rec(abc, [{a, 1}, {b, "2"}, {c, <<"3">>}]). 

bv_to_rec_test() ->
   #abc{a = 1, b = "2", c = <<"3">>} = ?bv_to_rec(abc, [{<<$a>>, 1}, {<<$b>>, "2"}, {<<$c>>, <<"3">>}]). 
