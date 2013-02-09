
%%
%% map record to key/val list
-define(rec_to_kv(Record, X),
   lists:zip(
      record_info(fields, Record),
      lists:map(
         fun(undefined) -> null; (E1234567890) -> E1234567890 end, 
         tl(tuple_to_list(X))
      )
   )
).

%%
%% map key/val list to record
-define(kv_to_rec(Record, X),
   list_to_tuple([Record |
      lists:map(
         fun(E1234567890) -> opts:val(E1234567890, undefined, X) end,
         record_info(fields, Record)
      )
   ])
).

%%
%% map key/val list to record
-define(bv_to_rec(Record, X),
   list_to_tuple([Record |
      lists:map(
         fun(E1234567890) -> opts:val(atom_to_binary(E1234567890, utf8), undefined, X) end,
         record_info(fields, Record)
      )
   ])
).

