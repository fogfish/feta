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
%% @description
%%   naive csv stream encoder / decoder (do not supported quoted fields yet). 
%%   The parser implements both state-full encoder / decoder interface and
%%   datum compatible streamer.
-module(csv).
-export([
   new/0
  ,new/1
  ,stream/1
  ,stream/2
  ,decode/2
  ,encode/2
]).

%%
%% types
-type(csv() :: [binary()]).

%% default options
-define(QUOTE,     <<$">>).
-define(FIELD_BY,  <<$,>>).
-define(LINE_BY,   <<$\n>>).

%%
%% parser state
-record(csv, {
   field  = ?FIELD_BY        %% field separator
  ,line   = ?LINE_BY         %% line  separator
  ,recbuf = <<>> :: binary() %% internal receive buffer
}).


%%
%% create csv parser
-spec(new/0 :: () -> #csv{}).
-spec(new/1 :: (list()) -> #csv{}).

new() ->
   #csv{}.

new(Opts) ->
   #csv{
      field = opts:val(field, ?FIELD_BY, Opts),
      line  = opts:val(line,  ?LINE_BY, Opts)
   }.

%%
%% stream decoder
-spec(stream/1 :: (stdio:stream()) -> stdio:stream()).
-spec(stream/2 :: (stdio:stream(), #csv{}) -> stdio:stream()).

stream(Stream) ->
   stream(Stream, new()).

stream({},     State) ->
   stdio:new();
stream(Stream, State) ->
   stream(stdio:head(Stream), Stream, State).

stream(Head, Stream, State0)
 when is_binary(Head) ->
   {Csv, State} = decode(Head, State0),
   stream(Csv, Stream, State);

stream([], Stream, State) ->
   stream(stdio:tail(Stream), State);
stream([Head|Tail], Stream, State) ->
   stdio:new(Head, fun() -> stream(Tail, Stream, State) end).


%%%------------------------------------------------------------------
%%%
%%% decoder
%%%
%%%------------------------------------------------------------------

%%
%% decode csv stream
%% returns parsed values and new parser state
-spec(decode/2 :: (binary(), #csv{}) -> {[csv()], #csv{}}).

decode(Chunk, #csv{recbuf = <<>>}=State)
 when is_binary(Chunk) ->
   decode(Chunk, [], State);

decode(Chunk, State)
 when is_binary(Chunk) ->
   decode(iolist_to_binary([State#csv.recbuf, Chunk]), [], State#csv{recbuf = <<>>}).

decode(Chunk, Acc, #csv{field=FieldBy, line=LineBy}=State) ->
   case binary:split(Chunk, LineBy) of
      [Head, Tail] ->
         decode(Tail, [split(Head, FieldBy) | Acc], State);
      [Head]      ->
         {lists:reverse(Acc), State#csv{recbuf = Head}}
   end.
   
%%
%% split csv line
split(Input, FieldBy) ->
   binary:split(Input, FieldBy, [global]).

%%%------------------------------------------------------------------
%%%
%%% encoder
%%%
%%%------------------------------------------------------------------

%%
%% encode csv stream
%% returns encoded values and new parser state
-spec(encode/2 :: (list() | tuple(), #csv{}) -> {binary(), #csv{}}).

encode([H|T], #csv{field=FieldBy, line=LineBy}=State) ->
   Msg = erlang:iolist_to_binary(
      [scalar:s(H), [[FieldBy, scalar:s(X)] || X <- T], LineBy]
   ),
   {Msg, State};

encode(Chunk, State)
 when is_tuple(Chunk) ->
   encode(erlang:tuple_to_list(Chunk), State).

