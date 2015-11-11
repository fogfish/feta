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
%%   scalar data type encode / decode utility 
%%
%%      built-in data type abbreviation: 
%%         a - atom, s - binary, c - list, i - integer, f - float
%%      
%%      extended data type set
%%         s - string (binary)
%%        ls - long string (synonym unicode:characters_to_binary + error handling)
%%         c - character list
%%        lc - long characters (synonym unicode:characters_to_list + error handling)
%%
%% @todo
%%   x(...) - unsigned int as a hexadecimal number.
%%   o(...) - unsigned int in octal.
%%
-module(scalar).

-export([
   i/1,     %% int as a signed decimal number.
   f/1,     %% double in normal (fixed-point) notation  
   s/1,     %% null-terminated string (binary in Erlang)
   ls/1,    %% null-terminated Unicode string 
   c/1,     %% char (character list)
   lc/1,    %% Unicode characters
   a/1,     %% existing atom
   atom/1,  %% new atom 
   decode/1 
]).

%%
%% scalar to integer
-spec(i/1 :: (any()) -> integer()).

i(X) when is_binary(X)  -> btoi(X);
i(X) when is_atom(X)    -> atoi(X);
i(X) when is_list(X)    -> ltoi(X);
i(X) when is_integer(X) -> X;
i(X) when is_float(X)   -> ftoi(X).

btoi(X) -> ltoi(btol(X)).
atoi(X) -> ltoi(atol(X)).
ltoi(X) -> list_to_integer(X).
ftoi(X) -> erlang:trunc(X).

%%
%% scalar to float
-spec(f/1 :: (any()) -> float()).

f(X) when is_binary(X)  -> btof(X);
f(X) when is_atom(X)    -> atof(X);
f(X) when is_list(X)    -> ltof(X);
f(X) when is_integer(X) -> itof(X);
f(X) when is_float(X)   -> X.

btof(X) -> ltof(btol(X)).
atof(X) -> ltof(atol(X)).
ltof(X) -> list_to_float(X).
itof(X) -> X + 0.0.

%%
%% scalar to string
-spec(s/1 :: (any()) -> binary()).

s(undefined)            -> <<>>;
s(X) when is_binary(X)  -> btos(X);
s(X) when is_atom(X)    -> atos(X);
s(X) when is_list(X)    -> ltos(X);
s(X) when is_integer(X) -> itos(X);
s(X) when is_float(X)   -> ftos(X).

btos(X) -> X.
atos(X) -> atom_to_binary(X, utf8).
ltos(X) -> iolist_to_binary(X).
itos(X) -> ltos(itol(X)).
ftos(X) -> ltos(io_lib:format("~.9f", [X])).

%%
%% scalar to Unicode string
-spec(ls/1 :: (any()) -> binary()).

ls(X) when is_binary(X)  -> utob(X);
ls(X) when is_atom(X)    -> atos(X);
ls(X) when is_list(X)    -> utob(X);
ls(X) when is_integer(X) -> itos(X);
ls(X) when is_float(X)   -> ftos(X).

utob(X) ->
   case unicode:characters_to_binary(X) of
      {incomplete, _} ->
         exit(rought);
      {error,      _} ->
         exit(badarg);
      Y ->
         Y
   end.


%%
%% character list
-spec(c/1 :: (any()) -> list()).

c(undefined)            -> [];
c(X) when is_binary(X)  -> btol(X);
c(X) when is_atom(X)    -> atol(X);
c(X) when is_list(X)    -> X;
c(X) when is_integer(X) -> itol(X);
c(X) when is_float(X)   -> ftol(X).

btol(X) -> binary_to_list(X).
atol(X) -> atom_to_list(X).
itol(X) -> integer_to_list(X).
ftol(X) -> lists:flatten(io_lib:format("~.9f", [X])).

%%
%% scalar to Unicode characters
-spec(lc/1 :: (any()) -> list()).

lc(X) when is_binary(X)  -> utoc(X);
lc(X) when is_atom(X)    -> atol(X);
lc(X) when is_list(X)    -> utoc(X);
lc(X) when is_integer(X) -> itol(X);
lc(X) when is_float(X)   -> ftol(X).

utoc(X) ->
   case unicode:characters_to_list(X) of
      {incomplete, _} ->
         exit(rought);
      {error,      _} ->
         exit(badarg);
      Y ->
         Y
   end.


%%
%% existing atom
-spec(a/1 :: (any()) -> atom()).

a(X) when is_binary(X)  -> btoa(X);
a(X) when is_atom(X)    -> X;
a(X) when is_list(X)    -> ltoa(X);
a(X) when is_integer(X) -> itoa(X);
a(X) when is_float(X)   -> ftoa(X).

btoa(X) -> binary_to_existing_atom(X, utf8).
ltoa(X) -> list_to_existing_atom(X).
itoa(X) -> ltoa(itol(X)).
ftoa(X) -> ltoa(ftol(X)).


%%
%% new atom
-spec(atom/1 :: (any()) -> atom()).

atom(X) when is_binary(X)  -> btoaa(X);
atom(X) when is_atom(X)    -> X;
atom(X) when is_list(X)    -> ltoaa(X);
atom(X) when is_integer(X) -> itoaa(X);
atom(X) when is_float(X)   -> ftoaa(X).

btoaa(X) -> binary_to_atom(X, utf8).
ltoaa(X) -> list_to_atom(X).
itoaa(X) -> ltoaa(itol(X)).
ftoaa(X) -> ltoaa(ftol(X)).


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
      nomatch               -> deref(X)
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

% binary is reference to part of large binary received from server
% copy binary to reduce size 
deref(X) ->
   case binary:referenced_byte_size(X) of
      Size when Size > 2 * byte_size(X) -> 
         binary:copy(X);
      _ -> 
         X
   end.


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------   

