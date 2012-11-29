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
%%   @description
%%      helper module to handle list of key/value options, extension to proplists
-module(opts).
-export([check/3, get/2, get/3]).

-type opts() :: [{atom(), term()} | atom()].

%%
%% check option list and add default option
-spec check(Key, Default, Opts) -> Opts when
      Key     :: atom(),
      Default :: term(),
      Opts    :: opts().

check(Key, Default, Opts) ->
   case lists:keyfind(Key, 1, Opts) of
      false -> [{Key, Default} | Opts];
      _     -> Opts
   end.

%%
%% read option value, throw {badarg, Key} error if key do not exists
-spec get(Key, Opts) -> Val when
      Key  :: atom(),
      Opts :: opts(),
      Val  :: term().

get(Key, Opts) ->
   case lists:keyfind(Key, 1, Opts) of
      {_, Val} -> Val;
      _        -> 
         case lists:member(Key, Opts) of
            true  -> true;
            false -> throw({badarg, Key})
         end
   end.

%%
%% read option value, return default key if key do not exists
-spec get(Key, Opts, Default) -> Val when
      Key  :: atom(),
      Opts :: opts(),
      Default :: term(),
      Val  :: term().

get(Key, Opts, Default) ->
   case lists:keyfind(Key, 1, Opts) of
      {Key, Val} -> Val;
      _          -> 
         case lists:member(Key, Opts) of
            true  -> true;
            false -> Default
         end
   end.
