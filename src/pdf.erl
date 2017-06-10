%%
%%   Copyright 2012 - 2015 Dmitry Kolesnikov, All Rights Reserved
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
%%  @doc
%%   probability distribution function
-module(pdf).

-export([
   poisson/1,
   pareto/2,
   pareto/3
]).

%%
%% Poisson distribution
%% (see https://en.wikipedia.org/wiki/Poisson_distribution)
poisson(L) ->
   X = math:exp(-L), 
   poisson(0, L, X, X, rand:uniform()).

poisson(X0, L, P0, S, U)
 when U > S ->
   X = X0 + 1,
   P = P0 * L / X,
   poisson(X, L, P, S + P, U);

poisson(X, _, _, _, _) ->
   X.

%%
%% Bounded Pareto distribution
pareto(A, H) ->
   pareto(A, 1, H).

pareto(A, L, H) ->
   U  = rand:uniform(),
   La = math:pow(L, A),
   Ha = math:pow(H, A),
   X  = math:pow( -(U * Ha - U * La - Ha) / (Ha * La), -1 / A ),
   erlang:trunc(X).

