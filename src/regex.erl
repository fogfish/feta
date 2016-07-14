%%
%%   Copyright 2012 - 2013 Dmitry Kolesnikov, All Rights Reserved
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
%%   regex helper
-module(regex).

-export([
   m/2,
   m/1,
   p/2,
   run/2
]).

-type(regex() :: re:mp() | binary() | list()).

%%
%% match string to regex
-spec m(binary(), regex()) -> nomatch | [binary()].

m(String, RegEx)
 when is_binary(String) ->
 	case re:run(String, RegEx) of
 		nomatch    -> 
 			nomatch;
 		{match, X} ->
 			[binary:part(String, At) || At <- X]
 	end.

%%
%% build regex match function
-spec m(regex()) -> function().

m(RegEx) ->
	{ok, Ex} = re:compile(RegEx),
 	fun(String) -> regex:m(String, Ex) end.

%%
%% build pattern match function, 
%% function is executed if input string matched pattern 
%% (bind function to regex)
-spec p(regex(), function()) -> function().

p(RegEx, Fun) ->
	{ok, Ex} = re:compile(RegEx),
	fun(String) ->
		case regex:m(String, Ex) of
			nomatch ->
				nomatch;
			Match   ->
				Fun(Match)
		end
	end.
	
%%
%% run regex application
-spec run(binary(), [function()]) -> nomatch | any().

run(String, [H | T])
 when is_binary(String) ->
 	case H(String) of
 		nomatch ->
 			regex:run(String, T);
 		Result  ->
 			Result
 	end;

run(_String, []) ->
 	nomatch.

