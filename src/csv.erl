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
  ,decode/1
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
  ,regex
  ,recbuf = <<>> :: binary() %% internal receive buffer
}).


%%
%% create csv parser
-spec new() -> #csv{}.
-spec new(list()) -> #csv{}.

new() ->
    Opts = [{line, ?LINE_BY},
            {field, ?FIELD_BY},
            {quote, ?QUOTE}],
    new(Opts).

new(Opts) ->
    Field =  opts:val(field, ?FIELD_BY, Opts),
    Line = opts:val(line,  ?LINE_BY, Opts),
    Quote = opts:val(qoute,  ?QUOTE, Opts),
    {ok, MP} = re:compile(<<"(",Quote/binary,")?",
                            "(?(1)",
                            "((",Quote/binary, "{2}|[^",Quote/binary,"])*",Quote/binary,")",
                            "|[^",Quote/binary, Field/binary, Line/binary,"]*)",
                            "(",Field/binary,"|",Line/binary, ")">>),
   #csv{
      field = Field,
      line = Line,
      regex = MP}.

%%
%% stream decoder
-spec stream(stdio:stream()) -> stdio:stream().
-spec stream(stdio:stream(), #csv{}) -> stdio:stream().

stream(Stream) ->
   stream(Stream, new()).

stream({},    _State) ->
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

-spec decode(#csv{}) -> {[csv()], #csv{}}.
decode(Chunk) ->
    decode(Chunk, new()).

-spec decode(binary(), #csv{}) -> {[csv()], #csv{}}.
decode(Chunk, #csv{recbuf = RecBuf, regex = Regex} = State)
 when is_binary(Chunk) ->
    NewChunk = iolist_to_binary([RecBuf, Chunk]),
    Match = re:run(NewChunk, Regex, [global, {capture, all, index}]),
    {ok, {_Csv, _NewState} = Result} = process_match(Match, State, NewChunk),
    Result.

-spec process_match({match, [binary:part()]} | nomatch, #csv{}, binary()) ->
    {ok, {[csv()], #csv{}}}.
process_match(nomatch, State, Chunk) ->
    {ok, {[], State#csv{recbuf = Chunk}}};

process_match({match, Matches}, State, Chunk) ->
    {Res, NewChunk} = process_chunk(Matches, State, Chunk),
    {ok, {Res, State#csv{recbuf = NewChunk}}}.

-spec process_chunk([binary:part()], #csv{}, binary()) ->
    {[csv()], binary()}.
process_chunk(Matches, #csv{line = LineBy} = State, Chunk) ->
    Matches2 = filter_incomplete_lines(Matches, Chunk, LineBy),
    process_chunk(Matches2, Chunk, State, [], [], 0).

process_chunk([], Chunk, #csv{}, [], Acc, LenProcessed) ->
    Size = byte_size(Chunk),
    NotProcessed = Size - LenProcessed,
    NewChunk = binary:part(Chunk, Size, -NotProcessed),
    {lists:reverse(Acc), NewChunk};

process_chunk([Match | Matches], Chunk,
              #csv{line = LineBy, field = FieldBy} = State, LineAcc, Acc, _) ->
    {Pos, Len} = hd(Match), % Match - {Pos, Len} - including delimeter
    Csv = binary:part(Chunk, Pos, Len - 1),
    Csv2 = format_term(Csv, State),
    Delim = binary:part(Chunk, Pos + Len, -1),
    case Delim of
        LineBy -> NewLine = lists:reverse([Csv2 | LineAcc]),
                  process_chunk(Matches, Chunk, State,
                                [], [NewLine | Acc], Pos+Len);
        FieldBy -> process_chunk(Matches, Chunk, State,
                                 [Csv | LineAcc], Acc, Pos+Len)
    end.
%%

filter_incomplete_lines(Matches, Chunk, LineBy) ->
    Fun = fun(Match) ->
                  PosLen = lists:last(Match),
                  binary:part(Chunk, PosLen) =/= LineBy
          end,
    lists:reverse(lists:dropwhile(Fun, lists:reverse(Matches))).

format_term(CsvTerm, #csv{line = LineBy}) ->
    Q = ?QUOTE,
    Term1 = case CsvTerm of
                <<Q:1/binary, Rest/binary>> ->
                    Size = byte_size(Rest) - 1,
                    <<Term:Size/binary, Q/binary>> = Rest,
                    Term;
                _ ->
                    CsvTerm
            end,
    Term2 = binary:replace(Term1, <<Q/binary, Q/binary>>, Q, [global]),
    binary:replace(Term2, LineBy, <<>>, [global]).


%%%------------------------------------------------------------------
%%%
%%% encoder
%%%
%%%------------------------------------------------------------------

%%
%% encode csv stream
%% returns encoded values and new parser state
-spec encode(list() | tuple(), #csv{}) -> {binary(), #csv{}}.

encode([H|T], #csv{field=FieldBy, line=LineBy}=State) ->
   Msg = erlang:iolist_to_binary(
      [scalar:s(H), [[FieldBy, scalar:s(X)] || X <- T], LineBy]
   ),
   {Msg, State};

encode(Chunk, State)
 when is_tuple(Chunk) ->
   encode(erlang:tuple_to_list(Chunk), State).

