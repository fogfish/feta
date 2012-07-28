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
-module(parser_tests).
-include_lib("eunit/include/eunit.hrl").


iso8601_test() ->
   {{2012,07,28}, _} = parser:iso8601("20120728"),
   {_,   {12,07,28}} = parser:iso8601("120728Z"),
   {_,   {12,07,28}} = parser:iso8601("120728"),
   {_,   {12,07,00}} = parser:iso8601("1207Z"),
   {_,   {12,07,00}} = parser:iso8601("1207"),
   {_,   {12,00,00}} = parser:iso8601("12Z"),
   {_,   {12,00,00}} = parser:iso8601("12"),
   {{2012,07,28}, {12,07,28}} = parser:iso8601("20120728T120728Z"),
   {{2012,07,28}, {12,07,28}} = parser:iso8601("20120728T120728"),
   {{2012,07,28}, {12,07,00}} = parser:iso8601("20120728T1207Z"),
   {{2012,07,28}, {12,07,00}} = parser:iso8601("20120728T1207"),
   {{2012,07,28}, {12,00,00}} = parser:iso8601("20120728T12Z"),
   {{2012,07,28}, {12,00,00}} = parser:iso8601("20120728T12").
