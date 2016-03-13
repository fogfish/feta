%% @description
%%   deflate i/o stream
-module(gz).

-export([
   stream/1
]).


%%
%% stream decoder
stream(Stream) ->
   Z = zlib:open(),
   zlib:inflateInit(Z, 31),
   stream(Stream, Z).

stream({},     Z) ->
   zlib:inflateEnd(Z),
   {};

stream(Stream, Z) ->
   Head = case zlib:inflate(Z, stdio:head(Stream)) of
      [Chunk] ->
         Chunk;
      IOList  ->
         erlang:iolist_to_binary(IOList)
   end,
   stdio:new(binary:copy(Head), fun() -> stream(stdio:tail(Stream), Z) end).

