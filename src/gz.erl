%% @description
%%   deflate i/o stream
-module(gz).

-export([
   stream/1
]).

-define(NULL,  undefined).


%%
%% stream decoder
stream(Stream) ->
   Z = zlib:open(),
   zlib:inflateInit(Z, 31),
   stream(Stream, Z).

stream(?NULL,     Z) ->
   zlib:inflateEnd(Z),
   zlib:close(Z),
   undefined;

stream(Stream, Z) ->
   Head = case zlib:inflate(Z, stdio:head(Stream)) of
      [Chunk] ->
         Chunk;
      IOList  ->
         erlang:iolist_to_binary(IOList)
   end,
   stdio:new(binary:copy(Head), fun() -> stream(stdio:tail(Stream), Z) end).

