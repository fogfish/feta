%%
%%   Copyright 2012 - 2014 Dmitry Kolesnikov, All Rights Reserved
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
%%   standard stream i/o library
-module(stdio).

-export([
   new/0
  ,new/1
  ,new/2
  ,head/1
  ,tail/1
  ,foreach/2
  ,list/1
  ,list/2
]).
-export([
   file/1
  ,file/2
]).

-export_type([stream/0]).

%%
%% data types and macro
-type(stream() :: {s, any(), function()}).

-define(NULL,  {}).


%%
%% creates a newly allocated stream - datum compatible
-spec(new/0 :: () -> stdio:stream()).
-spec(new/1 :: (any()) -> stream()).
-spec(new/2 :: (any(), function()) -> stream()).

new() ->
   ?NULL.
new(Head) ->
   new(Head, fun new/0).
new(Head, Fun)
 when is_function(Fun) ->
   {s, Head, Fun}.

%%
%% head element of stream
-spec(head/1 :: (stdio:stream()) -> any()).

head({s, Head, _}) ->
   Head;
head(_) ->
   exit(badarg).

%%
%% stream tail
-spec(tail/1 :: (stdio:stream()) -> stdio:stream()).

tail({s, _, Fun}) ->
   Fun();
tail(_) ->
   {}.

%%
%% apply function to each stream element
-spec(foreach/2 :: (function(), stdio:stream()) -> ok).

foreach(Fun, {s, _, _}=Stream) ->
   Fun(head(Stream)),
   foreach(Fun, tail(Stream));

foreach(_Fun, _) ->
   ok.

%%
%% return list of stream elements
-spec(list/1 :: (stdio:stream()) -> list()).
-spec(list/2 :: (integer(), stdio:stream()) -> list()).

list({s, _, _}=Stream) ->
   [stdio:head(Stream) | list(stdio:tail(Stream))];
list(_) ->
   [].

list(N, {s, _, _}=Stream)
 when N > 0 ->
   [stdio:head(Stream) | list(N - 1, stdio:tail(Stream))];
list(_, _) ->
   [].


%%
%% create file stream
%%  Options:
%%    * {iobuf, integer()} - size of i/o buffer
-spec(file/1 :: (list()) -> stdio:stream()).
-spec(file/2 :: (list(), list()) -> stdio:stream()).

file(File) ->
   file(File, []).

file(File, Opts) ->
   Chunk = opts:val(iobuf, 64 * 1024, Opts),
   {ok, FD} = file:open(File, [raw, binary, read, {read_ahead, Chunk}]),
   iostream(FD, Chunk).


iostream(FD, IoBuf)
 when is_tuple(FD), erlang:element(1, FD) =:= file_descriptor ->
   case file:read(FD, IoBuf) of
      {ok, Chunk} ->
         stdio:new(Chunk, fun() -> iostream(FD, IoBuf) end);
      eof  ->
         file:close(FD),
         stream:new();
      {error, Reason} ->
         file:close(FD),
         throw(Reason)
   end.


%%%------------------------------------------------------------------
%%%
%%% private 
%%%
%%%------------------------------------------------------------------



