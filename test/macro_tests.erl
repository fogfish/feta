-module(macro_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("feta/include/macro.hrl").

-record(abc, {a, b, c}).

pairs_test() ->
   [{a, 1}, {b, "2"}, {c, <<"3">>}]  = ?pairs(abc, #abc{a = 1, b = "2", c = <<"3">>}).

struct_1_test() ->
   #abc{a = 1, b = "2", c = <<"3">>} = ?structA(abc, [{a, 1}, {b, "2"}, {c, <<"3">>}]). 

struct_2_test() ->
   #abc{a = 1, b = "2", c = <<"3">>} = ?structB(abc, [{<<$a>>, 1}, {<<$b>>, "2"}, {<<$c>>, <<"3">>}]). 
