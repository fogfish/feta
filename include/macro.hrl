%%
%%   Copyright 2012 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @description
%%   common macro
%%
%% @todo
%%   * use lists:keyfind instead of proplists:get_value
-ifndef(ISO8601).
-define(ISO8601, "%Y%m%dT%H%M%SZ").
-endif.

-ifndef(UNXTIME).
-define(UNXTIME, "%s").
-endif.

%%
%% map record to list of key/val pairs
-ifndef(pairs).
-define(pairs(Struct, X),
   lists:zip(
      record_info(fields, Struct),
      tl(tuple_to_list(X))
   )
).
-endif.

%%
%% map key/val list to record
-ifndef(struct).
-define(struct(Struct, X),
   list_to_tuple([Struct |  [proplists:get_value(Y, X) || Y <- record_info(fields, Struct)]])
).
-endif.


%%
%% map key/val list to record (keys are atoms)
-ifndef(structA).
-define(structA(Struct, X),
   list_to_tuple([Struct |  [proplists:get_value(Y, X) || Y <- record_info(fields, Struct)]])
).
-endif.

%%
%% map key/val list to record (keys are binary)
-ifndef(structB).
-define(structB(Struct, X),
   list_to_tuple([Struct |  [proplists:get_value(atom_to_binary(Y, utf8), X) || Y <- record_info(fields, Struct)]])
).
-endif.
