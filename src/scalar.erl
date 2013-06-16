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
   s/1
]).

%%
%% scalar to string
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
