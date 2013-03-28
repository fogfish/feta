%%
-define(ISO8601, "%Y%m%dT%H%M%SZ").
-define(UNXTIME, "%s").

%%
%% map record to list of key/val pairs
-define(pairs(Struct, X),
   lists:zip(
      record_info(fields, Struct),
      tl(tuple_to_list(X))
   )
).

%%
%% map key/val list to record
-define(structA(Struct, X),
   list_to_tuple([Struct |  [proplists:get_value(Y, X) || Y <- record_info(fields, Struct)]])
).

%%
%% map key/val list to record
-define(structB(Struct, X),
   list_to_tuple([Struct |  [proplists:get_value(atom_to_binary(Y, utf8), X) || Y <- record_info(fields, Struct)]])
).
