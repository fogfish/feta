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
-export([check/3, get/2, get/3, val/2, val/3]).
-export_type([options/0]).

-type options() :: [{atom(), term()} | atom()].

%%
%% check option list and add default option
-spec check(Key, Default, Opts) -> Opts when
      Key     :: atom(),
      Default :: term(),
      Opts    :: options().

check(Key, Default, Opts) ->
   case lists:keyfind(Key, 1, Opts) of
      false -> [{Key, Default} | Opts];
      _     -> Opts
   end.

%%
%% read option value, throw {badarg, Key} error if key do not exists
-spec get(Key, Opts) -> {Key, Val} when
      Key  :: atom() | list(),
      Opts :: options(),
      Val  :: term().

get(Key, Opts) when is_atom(Key) ->
   case lists:keyfind(Key, 1, Opts) of
      {Key, _}=Val -> 
         Val;
      _  -> 
         case lists:member(Key, Opts) of
            true  -> Key;
            false -> throw({badarg, Key})
         end
   end;

get([Key|T], Opts) ->
   case opts:get(Key, Opts, undefined) of
      {_, undefined} -> get(T, Opts);
      Val            -> Val
   end;

get([], _Opts) ->
   throw(badarg).

%%
%% read option value, return default key if key do not exists
-spec get(Key, Opts, Default) -> {Key, Val} when
      Key  :: atom() | list(),
      Opts :: options(),
      Default :: term(),
      Val  :: term().

get(Key, Opts, Default) when is_atom(Key) ->
   case lists:keyfind(Key, 1, Opts) of
      {Key, _}=Val ->
         Val;
      _          -> 
         case lists:member(Key, Opts) of
            true  -> Key;
            false -> {Key, Default}
         end
   end;

get([Key|T], Opts, Default) ->
   case opts:get(Key, Opts, undefined) of
      {_, undefined} -> get(T, Opts, Default);
      Val            -> Val
   end;

get([], _Opts, Default) ->
   Default.

%%
%% read option value, throw {badarg, Key} error if key do not exists
-spec val(Key, Opts) -> Val when
      Key  :: atom(),
      Opts :: options(),
      Val  :: term().

val(Key, Opts) when is_atom(Key) ->
   case opts:get(Key, Opts) of
      {_, Val} -> Val;
      _        -> true
   end.

%%
%% read option value, return default key if key do not exists
-spec val(Key, Opts, Default) -> Val when
      Key  :: atom(),
      Opts :: options(),
      Default :: term(),
      Val  :: term().

val(Key, Opts, Default) when is_atom(Key) ->
   case opts:get(Key, Opts, Default) of
      {_, Val} -> Val;
      _        -> true
   end.
