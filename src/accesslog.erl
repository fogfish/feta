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
%%   access log parser 
%%   
%%     %...a:          Remote IP-address
%%     %...A:          Local IP-address
%%     %...B:          Bytes sent, excluding HTTP headers.
%%     %...b:          Bytes sent, excluding HTTP headers. In CLF format
%%                     i.e. a '-' rather than a 0 when no bytes are sent.
%% [x] %...c:          Connection status when response was completed.
%%                     'X' = connection aborted before the response completed.
%%                     '+' = connection may be kept alive after the response is sent.
%%                     '-' = connection will be closed after the response is sent.
%%     %...{FOOBAR}e:  The contents of the environment variable FOOBAR
%% [x] %...f:          Filename
%%     %...h:          Remote host
%% [x] %...H           The request protocol
%%     %...{Foobar}i:  The contents of Foobar: header line(s) in the request
%%                    sent to the server.
%%     %...l:          Remote logname (from identd, if supplied)
%%     %...m           The request method
%% [x] %...{Foobar}n:  The contents of note "Foobar" from another module.
%%     %...{Foobar}o:  The contents of Foobar: header line(s) in the reply.
%% [x] %...p:          The canonical Port of the server serving the request
%% [x] %...P:          The process ID of the child that serviced the request.
%% [x] %...q           The query string (prepended with a ? if a query string exists,
%%                     otherwise an empty string)
%%     %...r:          First line of request
%%     %...s:          Status.  For requests that got internally redirected, this is
%%                     the status of the *original* request --- %...>s for the last.
%%     %...t:          Time, in common log format time format (standard english format)
%%     %...{format}t:  The time, in the form given by format, which should
%%                     be in strftime(3) format. (potentially localized)
%%     %...T:          The time taken to serve the request, in seconds.
%%     %...u:          Remote user (from auth; may be bogus if return status (%s) is 401)
%% [x] %...U:          The URL path requested, not including any query string.
%%     %...v:          The canonical ServerName of the server serving the request.
%% [x] %...V:          The server name according to the UseCanonicalName setting.
-module(accesslog).

-export([
   decode/1
  ,decode/2
]).

%%
%% common log format
-define(COMMON_LOG, [$h, $l, $u, $t, $r, $s, $b]).

%%
%% decode access log
-spec(decode/1 :: (binary()) -> [{atom(), binary()}]).
-spec(decode/2 :: (list(), binary()) -> [{atom(), binary()}]).

decode(Log)
 when is_binary(Log) ->
   decode(?COMMON_LOG, Log).

decode([Head|Tail], Log)
 when is_binary(Log) ->
   {Token, Rest} = decode_token(Head, Log),
   [Token | decode(Tail, Rest)];

decode([], _Log) ->
   [].  

%%
%% decode single token of log line
decode_token($t, Log) ->
   %% decode common log format time [21/Mar/2000:14:08:03 -0600]
   [Head, Tail] = unquote(Log, <<$[>>, <<$]>>),
   %% @todo: decode time to tempus
   {pair($t, Head), Tail};

decode_token({$t, Fmt}, Log) ->
   case split(Log, <<$ >>, length(Fmt)) of
      X when length(X) =< length(Fmt) ->
         {pair($t, X), <<>>};
      X ->
         [Tail | Head] = lists:reverse(X),
         {pair($t, lists:reverse(Head)), Tail}
   end;

decode_token(T,  Log) ->
   case split(Log, <<$ >>) of
      [<<$", _/binary>>, _] ->
         [Head, Tail] = unquote(Log, <<$">>, <<$">>),
         {pair(T, Head), Tail};
      [Head, Tail] ->
         {pair(T, Head), Tail}
   end.

%%
%%
pair($a, X) -> {peeraddr, X};
pair($A, X) -> {addr,     X};
pair($B, X) -> {sent,     X};
pair($b, X) -> {sent,     X};
pair({$e, H}, X) -> {H,   X};
pair($h, X) -> {peerhost, X};
pair({$i, H}, X) -> {H,   X};
pair($l, X) -> {logname,  X};
pair($m, X) -> {method,   X};
pair({$o, H}, X) -> {H,   X};
pair($r, X) -> {request,  X};
pair($s, X) -> {status,   X};
pair($t, X) -> {time,     X};
pair($T, X) -> {latency,  X};
pair($u, X) -> {user,     X};
pair($v, X) -> {host,     X}.

%%
%% split binary, return head and tail
split(Bin, Pat) ->
   case binary:split(Bin, Pat) of
      [<<>>, X] ->
         split(X, Pat);   
      [X] ->
         [X, <<>>];
      X   ->
         X
   end.

split(<<>>,  _, _) ->
   [];
split(Bin,   _, 0) ->
   [Bin];
split(Bin, Pat, N) ->
   [Head, Tail] = split(Bin, Pat),
   [Head | split(Tail, Pat, N - 1)].

%%
%% unquote binary
unquote(Bin, Qa, Qb) ->
   case binary:split(Bin,  Qa) of
      [_, X] ->
         binary:split(X, Qb);
      X ->
         [<<>> | X]
   end.

