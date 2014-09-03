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
%%   N-triples encode / decode
-module(nt).

-export([
   decode/1
]).

-define(WS,  [<<$ >>, <<$\t>>, <<$\n>>]).
-define(EOL, <<$.>>).

%%
%%
-spec(decode/1 :: (binary()) -> [{binary(), binary(), binary()}]).

decode(Doc)
 when is_binary(Doc) ->
   case decode_statement(Doc) of
      {SPO, <<>>} ->
         [SPO];
      {SPO, Tail} ->
         [SPO | decode(Tail)]
   end.


decode_statement(X0) ->
   {S, X1} = decode_s(X0),
   {P, X2} = decode_p(X1),
   {O, X3} = decode_o(X2),
   case binary:split(X3, ?EOL) of
      [_, X4] ->
         {{S, P, O}, X4};
      [X4]    ->
         {{S, P, O}, X4}
   end.

%%
%%
decode_s(X) ->
   case split(X, ?WS) of
      {<<$_, $:, X/binary>>, Tail} ->
         {<<"urn:", X/binary>>, Tail};
      {Head, Tail} ->
         {Url,    _} = unquote(Head, <<$<>>, <<$>>>),
         {Url, Tail}
   end.

%%
%%
decode_p(X) ->
   {Head, Tail} = split(X, ?WS),
   {Url,     _} = unquote(Head, <<$<>>, <<$>>>),
   {Url,  Tail}.

%%
%%
decode_o(<<$<, _/binary>>=X) ->
   unquote(X, <<$<>>, <<$>>>);

decode_o(<<$", _/binary>>=X) ->
   case unquote(X, <<$">>, <<$">>) of
      {Head, <<$@, Rest/binary>>} ->
         %% @todo: use language tag
         {_Lang, Tail} = skip(Rest, ?EOL),
         {Head,  Tail};

      {Head, <<$^, $^, Rest/binary>>} ->
         %% @todo: use type for serialization 
         {_Type, Tail} = unquote(Rest, <<$<>>, <<$>>>),
         {Head,  Tail};

      {_Head, _Tail} = Result ->
         Result
   end.



%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%% split binary, return head and tail
split(Bin, Pat) ->
   case binary:split(Bin, Pat) of
      [<<>>, X] ->
         split(X, Pat);   
      [X] ->
         {X, <<>>};
      [H, T] ->
         {H, T}
   end.

%%
%% skip binary to pattern
skip(Bin, Pat) ->
   case binary:match(Bin, Pat) of
      [{0, _}] ->
         {<<>>, Bin};
      [{A, _}] ->
         {binary:part(Bin, 0, A - 1), binary:part(Bin, A)};
       _       ->
         {Bin, <<>>}
   end.

%%
%% unquote binary
unquote(Bin, Qa, Qb) ->
   case binary:split(Bin,  Qa) of
      [_, X] ->
         case binary:split(X, Qb) of
            [H, T] ->
               {H, T};
            [H] ->
               {H, <<>>}
         end;
      [X] ->
         {<<>>, X}
   end.

