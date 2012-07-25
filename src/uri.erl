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
%%    Implements http://tools.ietf.org/html/rfc3986
%%
%%
%%                userinfo
%%                __|___
%%               /      \
%%         foo://userinfo@example.com:8042/over/there?name=ferret#nose
%%         \_/            \______________/\_________/ \_________/ \__/
%%          |                    |            |            |        |
%%       scheme              authority       path        query   fragment
%%          |       path
%%          |   _____|__________________
%%         / \ /                        \
%%         urn:example:animal:ferret:nose
%%
%%    Serialized as nested tuple {uri, Schema, {...}}
%%     
%%
-module(uri).

-export([new/0, new/1, get/2, set/3, add/3, check/2, to_binary/1]).

-define(USER,  1).
-define(HOST,  2).
-define(PORT,  3).
-define(PATH,  4).
-define(QUERY, 5).
-define(FRAG,  6).

%%
%% new(URI) -> {uri, ...}
%%   URI       = list() | binary() | uri()
%%
%% parses URI into tuple, fails with badarg if invalid URI
%%
new() ->
   new(undefined).
new(Uri) when is_binary(Uri) ->
   {Schema, Body} = tokenize(Uri),
   {uri, Schema, Body};
new(Uri) when is_list(Uri) ->
   new(list_to_binary(Uri));
new(Schema) when is_atom(Schema) ->
   {uri, Schema, {<<>>, <<>>, undefined, <<>>, <<>>, <<>>}};
new({uri, _, _} = Uri) ->
   Uri.

%%
%% check([Elements], Uri) -> ok 
%%
%% validates that URI components is defined, fails badarg otherwise
check([], {uri, _,_} = Uri) ->
   Uri;
check([{Key, Val} | T], {uri, _,_} = Uri) ->
   case knet_uri:get(Key, Uri) of
      Val -> check(T, Uri);
      _   -> throw(badarg)
   end;
check([Key | T], {uri, _,_} = Uri) ->
   case knet_uri:get(Key, Uri) of
      <<>>      -> throw(badarg);
      undefined -> throw(badarg);
      _         -> check(T, Uri)
   end;
check(List, Uri) ->
   check(List, new(Uri)).   
   
   
%%
%% get(Item, Uri) -> binary() | integer()
%%
get(schema,   {uri, S, _}) ->  S;
get(userinfo, {uri, _, U}) -> erlang:element(?USER,  U);
get(host,     {uri, _, U}) -> erlang:element(?HOST,  U);
get(port,     {uri, S, U}) -> schema_to_port(S, erlang:element(?PORT, U));
get(authority,{uri, S, U}) -> {erlang:element(?HOST,  U), schema_to_port(S, erlang:element(?PORT, U))};
get(path,     {uri, _, U}) -> 
   case erlang:element(?PATH,  U) of
      <<>> -> <<$/>>;
      V    -> V
   end;
get(segments, {uri, _, _}=Uri) -> 
   [_ | Segs] = binary:split(uri:get(path, Uri), <<"/">>, [global, trim]),
   Segs;
get(q,        {uri, _, U}) -> erlang:element(?QUERY, U);
get(fragment, {uri, _, U}) -> erlang:element(?FRAG,  U);
get(Item, Uri) 
 when is_binary(Uri) orelse is_list(Uri) -> 
   uri:get(Item, new(Uri)).

%get(authority, {_, Uri}) when is_record(Uri, uri) ->
%   to_list(authority, Uri);
%get(resource, {_, Uri}) when is_record(Uri, uri) ->
%   to_list(resource, Uri);

%%
%% set(Item, V, Uri) -> NUri
%%
set(schema,   V, {uri, _, U}) when is_atom(V) -> 
   {uri, V, U};
set(userinfo, V, {uri, S, U}) when is_binary(V) -> 
   {uri, S, erlang:setelement(?USER, U, V)};
set(host,     V, {uri, S, U}) when is_binary(V) -> 
   {uri, S, erlang:setelement(?HOST, U, V)};
set(port,     V, {uri, S, U}) when is_integer(V) -> 
   {uri, S, erlang:setelement(?PORT, U, V)};   
set(authority, {Host, Port}, {uri, S, U}) when is_binary(Host), is_integer(Port) -> 
   {uri, S, erlang:setelement(?PORT, erlang:setelement(?HOST, U, Host), Port)};
set(authority,V, {uri, S, U}) when is_binary(V) -> 
   {Host,  Pbin} = suffix(V, <<$:>>),
   Port = case Pbin of
      <<>> ->  undefined;
      _    ->  list_to_integer(binary_to_list(Pbin))
   end,
   {uri, S, erlang:setelement(?PORT, erlang:setelement(?HOST, U, Host), Port)};
set(path,     V, {uri, S, U}) when is_binary(V) -> 
   {uri, S, erlang:setelement(?PATH, U, V)};
set(q,        V, {uri, S, U}) when is_binary(V) -> 
   {uri, S, erlang:setelement(?QUERY, U, V)};
set(fragment, V, {uri, S, U}) when is_binary(V) -> 
   {uri, S, erlang:setelement(?FRAG,  U, V)};
set(Item, {uri, _,_} = Src, {uri, _, _} = Dst) ->
   set(Item, get(Item, Src), Dst);
set(Item, V, Uri) when is_list(V) ->
   set(Item, list_to_binary(V), Uri);
set(Item, V, Uri) -> 
   set(Item, V, new(Uri)).   

%%
%% add(Item, V, Uri) -> NUri
add(path, V, {uri, _, _}=Uri) when is_binary(V) ->
   case uri:get(path, Uri) of
      <<$/>> -> 
         uri:set(path, <<$/, V/binary>>, Uri);
      Path   ->
         case binary:last(Path) of
            $/ -> 
               uri:set(path, <<Path/binary, V/binary>>, Uri);
            _  ->
               uri:set(path, <<Path/binary, $/, V/binary>>, Uri)
         end
   end;
add(Item, V, Uri) when is_list(V) ->
   add(Item, list_to_binary(V), Uri);
add(Item, V, Uri) -> 
   add(Item, V, new(Uri)).   

%%
%%
%%
to_binary({uri, S, {User, Host, Port, Path, Q, F}}) ->
   % schema
   Sbin = case is_list(S) of
      false -> <<(atom_to_binary(S, utf8))/binary, $:>>;
      true  -> <<(
         list_to_binary(
            string:join(
               lists:map(fun(X) -> atom_to_list(X) end, S),
               "+"
            )
         )), $:>>
   end,
   Ubin = case User of
     <<>> -> <<>>;
     _    -> <<User/binary, $@>>
   end,
   Pbin = case Port of
      undefined -> <<>>;
      _         -> <<$:, (list_to_binary(integer_to_list(Port)))/binary>>
   end,
   Auth = if
      Ubin =/= <<>> orelse Pbin =/= <<>> orelse Host =/= <<>> -> <<"//">>;
      true -> <<>>
   end,
   Qbin = if
      Q =/= <<>> -> <<$?, Q/binary>>;
      true -> <<>>
   end,
   Fbin = if
      F =/= <<>> -> <<$#, F/binary>>;
      true -> <<>>
   end,
   <<Sbin/binary, Auth/binary, Ubin/binary, Host/binary, Pbin/binary, Path/binary, Qbin/binary, Fbin/binary>>.
         
%%%------------------------------------------------------------------
%%%
%%% Private
%%%
%%%------------------------------------------------------------------

%%      URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
tokenize(Uri0) ->
   % uri
   {Sbin,  Uri1} = split(Uri0, <<$:>>),
   {Heir,   Uri2} = suffix(Uri1, <<$?>>),
   {Query,  Frag} = suffix(Uri2, <<$#>>),
   % heir
   {Auth0, Path} = case Heir of
      <<"//", H/binary>> -> 
         case suffix(H, <<$/>>) of
            {A, <<>>} -> {A, <<>>};
            {A,    P} -> {A, <<$/, P/binary>>}
         end;
      _                  -> {<<>>, Heir}
   end,
   {User, Host0} = prefix(Auth0, <<$@>>),
   {Host,  Pbin} = suffix(Host0, <<$:>>),
   % parse schema
   Scheme = case binary:split(Sbin, <<$+>>, [global]) of
      [S] -> binary_to_atom(S, utf8);
       S  -> lists:map(fun(X) -> binary_to_atom(X, utf8) end, S)
   end,
   % parse port
   Port = case Pbin of
      <<>> ->  undefined;
      _    ->  list_to_integer(binary_to_list(Pbin))
   end,
   {Scheme, {User, Host, Port, Path, Query, Frag}}.

   

split(Uri, T) ->
   case binary:split(Uri, T) of
      [Token, Rest] -> {Token, Rest};
      _             -> throw(baduri)
   end.
   
suffix(Uri, T) ->
   case binary:split(Uri, T) of
      [Token, Rest] -> {Token, Rest};
      _             -> {Uri,   <<>>}
   end.

prefix(Uri, T) ->
   case binary:split(Uri, T) of
      [Token, Rest] -> {Token, Rest};
      _             -> {<<>>,  Uri}
   end.   
   
%%
%% Maps schema to default ports

schema_to_port([S| _], P)   -> schema_to_port(S, P);
schema_to_port(tcp,    undefined) -> 80;   % custom schema for tcp sensors
schema_to_port(http,   undefined) -> 80;
schema_to_port(ws,     undefined) -> 80;
schema_to_port(ssl,    undefined) -> 443;  % custom schema for ssl sensors 
schema_to_port(https,  undefined) -> 443;
schema_to_port(wss,    undefined) -> 443;
schema_to_port(_,      undefined) -> throw(baduri);
schema_to_port(_,   Port) -> list_to_integer(binary_to_list(Port)).
   

