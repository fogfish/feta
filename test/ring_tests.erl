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
%%  @description
%%
-module(ring_tests).
-include_lib("eunit/include/eunit.hrl").

join_leave_by_addr_test() ->
   R0 = ring:new(),
   R1 = ring:join(<<0,0,0>>, localhost, R0),
   [localhost] = ring:members(R1),
   R2 = ring:leave(<<0,0,0>>, R1),
   [] = ring:members(R2).

key_test() ->
   Nodes = [
      {<<16#0,0>>, node0},
      {<<16#4,0>>, node1},
      {<<16#8,0>>, node2},
      {<<16#b,0>>, node3},
      {<<16#f,0>>, node4}
   ],
   R = lists:foldl(fun({Key, Node}, R) -> ring:join(Key, Node, R) end, ring:new(), Nodes),
   [node1] = ring:whereis(<<16#3, 0>>, R),
   [node2] = ring:whereis(<<16#5, 0>>, R),
   [node0] = ring:whereis(<<16#f, 1>>, R).