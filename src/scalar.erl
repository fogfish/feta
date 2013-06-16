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
%%   @description
%%      scalar data type utility, 
%%      built-in data type abbreviation: 
%%         a - atom, b - binary, l - list, i - integer, f - float
%%      
%%      extended data type set
%%         s - string
-module(scalar).

-export([
   decode/1, s/1
]).

%%
%% decode scalar type
-spec(decode/1 :: (list() | binary()) -> any()).

decode(<<"true">>)  ->
   true;
decode(<<"false">>) ->
   false;
decode(X)
 when is_binary(X) ->
   case re:run(X, "^(-?[0-9]+)(\\.[0-9]+)?([eE][+-]?[0-9])?$") of
      {match, [_, _]}       -> btoi(X);
      {match, [_, _, _]}    -> btof(X); 
      {match, [_, _, _, _]} -> btof(X);
      nomatch -> 
         % binary is reference to part of large binary received from server
         % copy binary to reduce size 
         case binary:referenced_byte_size(X) of
            Size when Size > 2 * byte_size(X) -> 
               binary:copy(X);
            _ -> 
               X
         end
   end;
decode("true") -> 
   true;
decode("false") -> 
   false;
decode(X)
 when is_list(X) ->
   case re:run(X, "^(-?[0-9]+)(\\.[0-9]+)?([eE][+-]?[0-9])?$") of
      {match, [_, _]}       -> ltoi(X);
      {match, [_, _, _]}    -> ltof(X); 
      {match, [_, _, _, _]} -> ltof(X);
      nomatch -> X
   end.


%%
%% scalar to string
-spec(s/1 :: (any()) -> list()).
s(X) when is_atom(X)    -> atos(X);
s(X) when is_list(X)    -> ltos(X);
s(X) when is_integer(X) -> itos(X);
s(X) when is_float(X)   -> ftos(X).

%%
%% to string
atos(X) -> atom_to_binary(X, utf8).
ltos(X) -> list_to_binary(X).
itos(X) -> ltos(itol(X)).
ftos(X) -> ltos(io_lib:format("~.9f", [X])).

%%
%% to list
itol(X) -> integer_to_list(X).
btol(X) -> binary_to_list(X).

%%
%% from list
ltoi(X) -> list_to_integer(X).
ltof(X) -> list_to_float(X).

%%
%% from binary
btoi(X) -> ltoi(btol(X)).
btof(X) -> ltof(btol(X)).



