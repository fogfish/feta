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
-module(format_tests).
-include_lib("eunit/include/eunit.hrl").

%%
%% Day formating
'%a_test'() ->
   "Thu" = format:datetime("%a", {{2012,07,26}, {23,24,00}}),
   {{0,0,0},{0,0,0}} = parser:datetime("%a", "Thu").

'%A_test'() ->
   "Thursday" = format:datetime("%A", {{2012,07,26}, {23,24,00}}),
   {{0,0,0},{0,0,0}} = parser:datetime("%A", "Thursday").

'%d_test'() ->
   "26" = format:datetime("%d", {{2012,07,26}, {23,24,00}}),
   "01" = format:datetime("%d", {{2012,07,01}, {23,24,00}}),
   {{0,0,26},{0,0,0}} = parser:datetime("%d", "26"),
   {{0,0, 1},{0,0,0}} = parser:datetime("%d", "01").

'%e_test'() ->
   "26" = format:datetime("%e", {{2012,07,26}, {23,24,00}}),
   " 1" = format:datetime("%e", {{2012,07,01}, {23,24,00}}),
   {{0,0,26},{0,0,0}} = parser:datetime("%e", "26"),
   {{0,0, 1},{0,0,0}} = parser:datetime("%e", " 1").

'%j_test'() ->
   "031" = format:datetime("%j", {{2012,01,31}, {00,00,00}}),
   {{0,0,0},{0,0,0}} = parser:datetime("%j", "031").

'%u_test'() ->
   "4" = format:datetime("%u", {{2012,07,26}, {23,24,00}}),
   {{0,0,0},{0,0,0}} = parser:datetime("%u", "4").

'%w_test'() ->
   "4" = format:datetime("%w", {{2012,07,26}, {23,24,00}}),
   {{0,0,0},{0,0,0}} = parser:datetime("%w", "4").


%%
%% Month formating
'%b_test'() ->
   "Jul" = format:datetime("%b", {{2012,07,26}, {23,24,00}}),
   {{0,7,0},{0,0,0}} = parser:datetime("%b", "Jul").

'%B_test'() ->
   "July" = format:datetime("%B", {{2012,07,26}, {23,24,00}}),
   {{0,7,0},{0,0,0}} = parser:datetime("%B", "July").

'%m_test'() ->
   "10" = format:datetime("%m", {{2012,10,26}, {23,24,00}}),
   "07" = format:datetime("%m", {{2012,07,01}, {23,24,00}}),
   {{0,10,0},{0,0,0}} = parser:datetime("%m", "10"),
   {{0, 7,0},{0,0,0}} = parser:datetime("%m", "07").

%%
%% Year formating
'%y_test'() ->
   "02" = format:datetime("%y", {{2002,10,26}, {23,24,00}}),
   {{2002,0,0},{0,0,0}} = parser:datetime("%y", "02").


'%Y_test'() ->
   "2012" = format:datetime("%Y", {{2012,10,26}, {23,24,00}}),
   {{2012,0,0},{0,0,0}} = parser:datetime("%Y", "2012").


%%
%% Time formating
'%H_test'() ->
   "23" = format:datetime("%H", {{2012,10,26}, {23,00,00}}),
   "07" = format:datetime("%H", {{2012,07,01}, {07,00,00}}),
   {{0,0,0},{23,0,0}} = parser:datetime("%H", "23"),
   {{0,0,0},{ 7,0,0}} = parser:datetime("%H", "07").


'%M_test'() ->
   "23" = format:datetime("%M", {{2012,10,26}, {00,23,00}}),
   "07" = format:datetime("%M", {{2012,07,01}, {00,07,00}}),
   {{0,0,0},{0,23,0}} = parser:datetime("%M", "23"),
   {{0,0,0},{0, 7,0}} = parser:datetime("%M", "07").


'%S_test'() ->
   "23" = format:datetime("%S", {{2012,10,26}, {00,00,23}}),
   "07" = format:datetime("%S", {{2012,07,01}, {00,00,07}}),
   {{0,0,0},{0,0,23}} = parser:datetime("%S", "23"),
   {{0,0,0},{0,0, 7}} = parser:datetime("%S", "07").

%%
%% Timestamp format
'%s_test'() ->
   "1343469600" = format:datetime("%s", {{2012,07,28}, {10,00,00}}),
   {{2012,07,28}, {10,00,00}} = parser:datetime("%s", "1343469600").


%%
%% Date time formating
datetime_test() ->
   "2012-10-26 12:23:05" = format:datetime(
   	"%Y-%m-%d %H:%M:%S", 
   	{{2012,10,26}, {12,23,05}}
   ),
   "26-Oct-2012 12:23:05" = format:datetime(
   	"%d-%b-%Y %H:%M:%S", 
   	{{2012,10,26}, {12,23,05}}
   ),
   {{2012,10,26}, {12,23,05}} = parser:datetime(
   	"%Y-%m-%d %H:%M:%S", 
   	"2012-10-26 12:23:05"

   ),
   {{2012,10,26}, {12,23,05}} = parser:datetime(
   	"%d-%b-%Y %H:%M:%S", 
      "26-Oct-2012 12:23:05"
   ).

