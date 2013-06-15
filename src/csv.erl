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
%%    The simple CSV-file parser based on event model. The parser generates an
%%    event/callback when the CSV line is parsed. The parser supports both
%%    sequential and parallel parsing.
%%
%%                           Acc
%%                       +--------+
%%                       |        |
%%                       V        |
%%                  +---------+   |
%%    ----Input---->| Parser  |--------> AccN
%%          +       +---------+
%%         Acc0          |
%%                       V
%%                   Event Line 
%%
%%    The parser takes as input binary stream, event handler function and
%%    initial state/accumulator. Event function is evaluated agains current 
%%    accumulator and parsed line of csv-file. Note: The accumaltor allows to
%%    carry-on application specific state throught event functions.
%%
-module(csv).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').
-export([
   encode/1, decode/1,

   parse/3, split/4, pparse/4, stream/1,
   infile/3
]).

%%
%%
-define(QUOTE,     $").
-define(FIELD_BY,  $,).
-define(LINE_BY,   $\n).
-define(IO_CHUNK,  1024*1024).

-record(csv, {
   data  = <<>>  :: binary(),  %% partially parsed data
   line  = []    :: list(),    %% partially parsed line
   file  = []    :: list()     %% parsed line's accumulator
}). 

%%
%%
encode(Data)
 when is_list(Data) ->
   iolist_to_binary([encode_line(X) || X <- Data]).

encode_line(L)
 when is_tuple(L) ->
   encode_line(tuple_to_list(L));
encode_line([]) ->
   [];
encode_line([H|T]) ->
   [encode_value(H), lists:append([[?FIELD_BY, encode_value(X)] || X <- T]), ?LINE_BY].

encode_value(X)
 when is_binary(X) ->
   X;
encode_value(X)
 when is_list(X) ->
   iolist_to_binary(X);
encode_value(X) 
 when is_atom(X) ->
   atom_to_binary(X, utf8);
encode_value(X)
 when is_integer(X) ->
   list_to_binary(integer_to_list(X)).

%%
%%
decode(CSV) ->
   lists:reverse(
      parse(CSV, fun decode_line/2, [])
   ).

decode_line({line, L}, Acc) ->
   [list_to_tuple(lists:reverse(L)) | Acc];
decode_line(_, Acc) ->
   Acc.



%%
%% parse(In, Fun, Acc0) -> Acc
%%   In  = binary(), input csv data to parse
%%   Fun = fun({line, Line}, Acc0) -> Acc, 
%%      Line  - list() list of parsed fields in reverse order
%%   Acc0 = term() application specific state/term carried throught
%%                 parser event hadlers
%%
%% sequentially parses csv file
%%
parse(In, Fun, Acc0) when is_list(In) ->
   % io_list, each item has to be parsed
   lists:foldl(fun(X, A) -> parse(X, Fun, A) end, Acc0, In); 
parse(In, Fun, Acc0) when is_binary(In) ->
   % pure binary
   parse(In, 0, 0, [], Fun, Acc0).

%%
%% parse(In, Nbr, Pos, Len, Line, FUn, Acc0) ->
%%   Nbr = current line number
%%   Pos = token position at input chunk, 
%%   Len = token length
%%   Line = accumulator of parsed tokens
%%   Fun = event handler
%%   Acc0 = accumulator (state of event handler) 
parse(In, Pos, Len, Line, Fun, Acc0)
 when Pos + Len < size(In) ->
   case In of
      % start of quoted field
      <<_:Pos/binary, _Tkn:Len/binary, ?QUOTE, _/binary>> ->
         parse_quoted(In, Pos + Len + 1, 0, Line, Fun, Acc0);
      % end of field
      <<_:Pos/binary, Tkn:Len/binary, ?FIELD_BY,  _/binary>> ->
         parse(In, Pos + Len + 1, 0, [Tkn | Line], Fun, Acc0);
      % last line
      <<_:Pos/binary, Tkn:Len/binary, ?LINE_BY>> ->
         % last line match
         Fun(eof, Fun({line, [Tkn | Line]}, Acc0));
      % end of line
      <<_:Pos/binary, Tkn:Len/binary, ?LINE_BY, _/binary>>  ->
         % line match
         parse(In, Pos + Len + 1, 0, [], 
               Fun, Fun({line, [Tkn | Line]}, Acc0));
      _ ->
         % no match increase token
         parse(In, Pos, Len + 1, Line, Fun, Acc0)
   end;
parse(In, Pos, Len, Line, Fun, Acc0) ->
   <<_:Pos/binary, Tkn:Len/binary, _/binary>> = In,
   Fun(eof, Fun({line, [Tkn | Line]}, Acc0)).
  
parse_quoted(In, Pos, Len, Line, Fun, Acc0) ->
   case In of
      <<_:Pos/binary, Tkn:Len/binary, ?QUOTE, ?QUOTE, _/binary>> ->
         parse_quoted(In, Pos, Len + 2, Line, Fun, Acc0);
      <<_:Pos/binary, Tkn:Len/binary, ?QUOTE, ?FIELD_BY, _/binary>> ->
         % field match
         parse(In, Pos + Len + 2, 0, [unescape(Tkn) | Line], Fun, Acc0);
      <<_:Pos/binary, Tkn:Len/binary, ?QUOTE, ?LINE_BY, _/binary>> ->
         % field match
         parse(In, Pos + Len + 2, 0, [], Fun, 
               Fun({line, [unescape(Tkn) | Line]}, Acc0));   
      _ ->   
         parse_quoted(In, Pos, Len + 1, Line, Fun, Acc0)
   end.   
         
%%
%% unescape
unescape(In) ->
   unescape(In, 0, 0, <<>>).
   
unescape(In, I, Len, Acc) when I + Len < size(In) ->
   case In of
      <<_:I/binary, Tkn:Len/binary, ?QUOTE, ?QUOTE, _/binary>> ->
         unescape(In, I + Len + 2, 0, <<Acc/binary, Tkn/binary, ?QUOTE>>);
      _ ->
         unescape(In, I, Len + 1, Acc)
   end;
unescape(In, I, Len, Acc) ->
   <<_:I/binary, Tkn:Len/binary>> = In,
   <<Acc/binary, Tkn/binary>>.      
   
%%
%% split(In, Count, Fun, Acc0) -> Acc0
%%    In    = binary(), input csv data to split
%%    Count = integer(), number of shard to produce
%%    Fun = fun({shard, Shard}, Acc0) -> Acc, 
%%       Shard  - binary() chunk of csv data
%%    Acc0 = term() application specific state/term carried throught
%%                  parser event hadlers
%%
%% split csv file on chunks
%%
split(In, Count, Fun, Acc0) when is_list(In) ->
   %io_list, each chunk is handled at own process, count is ignored
   lists:foldl(fun(X, A) -> Fun({shard, X}, A) end, Acc0, In);
split(In, Count, Fun, Acc0) when is_binary(In) ->
   Size = erlang:round(size(In) / Count), % approximate a shard size
   split(In, 0, Size, Size, Fun, Acc0).
 
split(In, Pos, Size, Size0, Fun, Acc0) when Pos + Size < size(In) ->
   case In of
      <<_:Pos/binary, Shard:Size/binary, ?LINE_BY>> ->
         Fun({shard, Shard}, Acc0);
      <<_:Pos/binary, Shard:Size/binary, ?LINE_BY, _/binary>> ->
         split(In, Pos + Size + 1, Size0,    Size0, Fun, 
            Fun({shard, Shard}, Acc0)
         );
      _ ->
         split(In, Pos, Size + 1, Size0, Fun, Acc0)
   end;
split(In, Pos, _Size, _Size0, Fun, Acc0) ->
   <<_:Pos/binary, Shard/binary>> = In,
   Fun({shard, Shard}, Acc0).

%%
%% pparse(In, Count, Fun, App) -> NApp
%%   In    = binary(), input csv data to parse
%%   Count = integers(), defines a number of worker processes
%%   Fun   = fun({line, Line}, Acc0) -> Acc, 
%%      Line  - list() list of parsed fields in reverse order
%%   Acc0 = term() application specific state/term carried throught
%%                 parser event hadlers
%%
%% parallel parse csv file, the function shards the input csv data and
%% parses each chunk in own process.
%%
pparse(In, Count, Fun, Acc0) ->   
   % pure binary
   Wrk = fun({shard, Shard}, Id) ->
      Pid = self(),
      spawn(
         fun() ->
            R = parse(Shard, Fun, Fun({shard, Shard}, Acc0)),
            Pid ! {shard, Id, R}
         end
      ),
      Id + 1
   end,
   N = split(In, Count, Wrk, 1),
   join(lists:seq(1,N - 1), []).

   
join([H | T], Acc) ->
   receive 
      {shard, H, R} when is_list(R) -> join(T, Acc ++ R);
      {shard, H, R} -> join(T, [R|Acc])
   end;
join([], Acc) ->
   Acc.


%%
%% infile(Filename, Fun, Acc0) -> {ok, Acc} | {error, ...}
%%   Filename = list(), input csv data to parse
%%   Fun = fun({line, Line}, Acc0) -> Acc, 
%%      Line  - list() list of parsed fields in reverse order
%%   Acc0 = term() application specific state/term carried throught
%%                 parser event hadlers
%%
%% sequentially parses csv file
%%
infile(Filename, Fun, Acc0) ->
   case file:open(Filename, [read, binary]) of
      {ok, File} ->
         Ret = input(File, <<>>, Fun, Acc0),
         file:close(File),
         Ret;
      Err -> Err
   end.

input(File, Prefix, Fun, Acc0) ->
   case file:read(File, ?IO_CHUNK) of
      eof ->
         {ok, Acc0};
      {ok, Chnk} ->
         {CSV, Suffix} = chunk(<<Prefix/binary, Chnk/binary>>),
         input(File, Suffix, Fun, parse(CSV, Fun, Acc0));
      Err ->
         Err
   end.
         
%%
%% binary split by most right new line 
chunk(In) ->
   chunk(In, size(In)).
    
chunk(In, Len) when Len > 0 ->
   case In of
      <<Chnk:Len/binary, ?LINE_BY, Sfx/binary>> ->
         {<<Chnk/binary, ?LINE_BY>>, Sfx};
      _ ->
         chunk(In, Len - 1)
   end;
chunk(In, _) ->
   {<<>>, In}.                  


%%
%% lazy evaluation of csv stream
stream(Series) ->
   lazy:unfold(
      fun(In, #csv{data=Pfx}=S) -> 
         sparse(<<Pfx/binary, In/binary>>, 0, 0, S#csv{data = <<>>}) 
      end,
      #csv{},
      Series
   ).

sparse(In, Pos, Len, #csv{line=Line, file=File}=S)
 when Pos + Len < size(In) ->
   case In of
      % end of field
      <<_:Pos/binary, Tkn:Len/binary, ?FIELD_BY,  _/binary>> ->
         sparse(In, Pos + Len + 1, 0, S#csv{line=[Tkn | Line]});
      % end of line
      <<_:Pos/binary, Tkn:Len/binary, ?LINE_BY, _/binary>>  ->
         sparse(In, Pos + Len + 1, 0, S#csv{line=[], file=[[Tkn | Line]|File]});
      _ ->
         % no match increase token
         sparse(In, Pos, Len + 1, S)
   end;

sparse(In, Pos, _Len, #csv{file=File}=S) ->
   <<_:Pos/binary, Sfx/binary>> = In,
   {lists:reverse(File), S#csv{data=Sfx, file=[]}}.


   
   

