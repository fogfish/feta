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
%%    implements http://tools.ietf.org/html/rfc3986
%%
%%
%%                userinfo
%%                __|___
%%               /      \
%%         foo://userinfo@example.com:8042/over/there?name=ferret#nose
%%         \_/            \______________/\_________/ \_________/ \__/
%%          |                    |            |            |        |
%%       scheme              authority       path        query   fragment(anchor)
%%          |       path
%%          |   _____|__________________
%%         / \ /                        \
%%         urn:example:animal:ferret:nose
%%
%%    Serialized as nested tuple {uri, Schema, {...}}
%%     
%%
%% @todo
%%   * escape works badly when URI is copied (proper escaping)
-module(uri).

-export([
   new/0, 
   new/1,
   schema/1,
   schema/2,
   userinfo/1,
   userinfo/2,
   host/1,
   host/2,
   port/1,
   port/2,
   authority/1,
   authority/2,
   path/1,
   path/2,
   segments/1,
   segments/2,
   join/2,
   q/1,
   q/2,
   q/3,
   anchor/1,
   anchor/2,
   suburi/1,
   suburi/2,
   get/2,
   set/3,
   check/2,
   % helper functions
   s/1,
   c/1,
   urn/2,
   unescape/1, 
   escape/1,
   aton/1,
   registry/2
]).

-export_type([uri/0, urn/0]).

%% 
-type(uri()    :: {uri, schema(),  any()}).
-type(urn()    :: {urn, binary(), binary}).
-type(schema() :: binary() | atom() | [atom()]).

%% internal uri structures
-record(uval, {
   user  = undefined :: binary(),
   host  = undefined :: binary(),
   port  = undefined :: integer(),
   path  = undefined :: binary(),
   q     = undefined :: binary(),
   anchor= undefined :: binary() 
}).

%% query operation
-define(COMPARE, [<<$=, $<>>, <<$>, $=>>, <<$>>>, <<$<>>, <<$=>>]).
-define(is_uri(X),  (X =:= uri orelse X =:= urn)).
-define(is_url(X),  (X =:= uri)).
-define(is_urn(X),   X =:= urn).
%%
%% parses URI into tuple, fails with badarg if invalid URI
-spec new() -> uri().
-spec new(list() | binary() | atom() | uri()) -> uri().

new() ->
   new(undefined).

new(Uri)
 when is_binary(Uri) ->
   case parse_uri(Uri) of
      {urn, UVal} ->
         case binary:split(UVal#uval.path, <<$:>>) of
            [Schema, Path] ->
               case binary:split(Schema, <<$+>>, [global]) of
                  [S] ->  {urn, S, Path};
                   S  ->  {urn, S, Path}
               end;
            [Schema] ->
               {urn, Schema, <<>>}
         end;
      {Schema, UVal} ->
         {uri, Schema, UVal}
   end;

new(Uri)
 when is_list(Uri) ->
   new(list_to_binary(Uri));

new(Schema)
 when is_atom(Schema) ->
   {uri, Schema, #uval{}};

new({uri, _, _} = Uri) ->
   Uri;
new({urn, _, _} = Urn) ->
   Urn.


%%%------------------------------------------------------------------
%%%
%%% setter / getters
%%%
%%%------------------------------------------------------------------   

%%
%% uri schema
-spec schema(uri()) -> schema().
-spec schema(schema(), uri()) -> uri().

schema({Uri, S, _})
 when ?is_uri(Uri) ->
   S.

schema(Val, {Uri, _, U})
 when ?is_urn(Uri), (is_atom(Val) orelse is_binary(Val)) ->
   {Uri, scalar:s(Val), U}; 

schema(Val, {Uri, _, U})
 when ?is_uri(Uri), (is_atom(Val) orelse is_list(Val)) ->
   {Uri, Val, U}.


%%
%% uri user info
-spec userinfo(uri()) -> {binary(), binary()} | binary().
-spec userinfo({any(), any()} | any(), uri()) -> uri().

userinfo({_, _, #uval{user=undefined}}) ->
   undefined;
userinfo({_, _, #uval{}=U}) ->
   case binary:split(U#uval.user, <<$:>>) of
      [User, Pass] -> {unescape(User), unescape(Pass)};
      [Info]       -> unescape(Info)
   end.

userinfo(undefined, {Uri, S, #uval{}=U}) ->
   {Uri, S, U#uval{user = undefined}};
userinfo({User0, Pass0}, {Uri, S, #uval{}=U}) ->
   User = escape(scalar:s(User0)),
   Pass = escape(scalar:s(Pass0)),
   {Uri, S, U#uval{user = <<User/binary, $:, Pass/binary>>}};
userinfo(Val, {Uri, S, #uval{}=U}) ->
   {Uri, S, U#uval{user = escape(scalar:s(Val))}}.

%%
%%
-spec host(uri()) -> binary().
-spec host(any(), uri()) -> uri().

host({_,  _, #uval{}=U}) ->
   U#uval.host.

host(undefined, {Uri, S, #uval{}=U}) ->
   {Uri, S, U#uval{host = undefined}};
host(Val, {_, _, _}=Uri)
 when is_tuple(Val) ->
   host(inet_parse:ntoa(Val), Uri);
host(Val, {Uri, S, #uval{}=U}) ->
   {Uri, S, U#uval{host = scalar:s(Val)}}.

%%
%%
-spec port(uri()) -> integer().
-spec port(any(), uri()) -> uri().

port({_, S, #uval{}=U}) ->
   schema_to_port(S, U#uval.port).

port(undefined, {Uri, S, #uval{}=U}) ->
   {Uri, S, U#uval{port = undefined}};
port(Val, {Uri, S, #uval{}=U}) ->
   {Uri, S, U#uval{port = scalar:i(Val)}}.

%%
%% authority   = [ userinfo "@" ] host [ ":" port ]
-spec authority(uri()) -> {binary(), integer()}.
-spec authority({any(), any()} | any(), uri()) -> uri().

authority({_, _, _}=Uri) ->
   case {uri:host(Uri), uri:port(Uri)} of
      {undefined, undefined} ->
         undefined;
      Value ->
         Value
   end.

authority(undefined, Uri) ->
   uri:port(undefined, uri:host(undefined, Uri));

authority({Host, undefined}, {uri, _, _}=Uri) ->
   uri:host(Host, Uri);
authority({Host, Port}, {uri, _, _}=Uri) ->
   uri:port(scalar:i(Port), uri:host(Host, Uri));

authority(Val, {uri, _, _}=Uri) ->
   {User, Auth} = prefix(scalar:s(Val),  <<$@>>),
   {Host, Port} = suffix(Auth, <<$:>>),
   uri:userinfo(User, uri:authority({Host, Port}, Uri)).

%%
%%
-spec path(uri()) -> binary().
-spec path(any(), uri()) -> uri().

path({_, _, #uval{path=undefined}}) ->
   undefined;
path({_, _, #uval{}=U}) ->
   case unescape(U#uval.path) of
      <<>> -> <<$/>>;
      V    -> V
   end;
path({urn, _, Path}) ->
   Path.


path(undefined, {Uri, S, #uval{}=U}) ->
   {Uri, S, U#uval{path = undefined}};   
path(Val, {Uri, S, #uval{}=U}) ->
   % @todo: do we need to escape path?
   {Uri, S, U#uval{path = scalar:s(Val)}};
path(Val, {urn, S, _}) ->
   {urn, S, scalar:s(Val)}.

%%
%%
-spec segments(uri()) -> [binary()].
-spec segments(any(), uri()) -> uri().

segments({_, _, #uval{path=undefined}}) ->
   undefined;
segments({_, _, #uval{path=Path}}) ->
   case binary:split(Path, <<"/">>, [global, trim]) of
      []            -> [];
      [<<>> | Segs] -> Segs;
      Segs          -> Segs
   end;

segments({urn,  _, undefined}) ->
   undefined;
segments({urn,  _, Path}) ->
   binary:split(Path, [<<":">>, <<"/">>], [global, trim]).


segments(undefined, Uri) ->
   uri:path(undefined, Uri);

segments(Val, {uri, _, _} = Uri)
 when is_list(Val) ->
   uri:path(iolist_to_binary([[$/, escape(X)] || X <- Val]), Uri);

segments(Val, {urn, _, _} = Uri)
 when is_list(Val) ->
   uri:path(iolist_to_binary([escape(hd(Val)) | [[$:, escape(X)] || X <- tl(Val)]]), Uri);


segments(Val, Uri)
 when is_tuple(Val) ->
   uri:segments(tuple_to_list(Val), Uri).

%%
%% join path segment(s)
-spec join([any()], uri()) -> uri().

join([H|T], {urn, _, _}=Uri) ->
   X = iolist_to_binary([scalar:s(H)|[[$:, scalar:s(X)] || X <- T]]),
   case uri:path(Uri) of
      <<>> ->
         uri:path(X, Uri);
      Path ->
         uri:path(<<Path/binary, $:, X/binary>>, Uri)
   end;

join(Join, {T, _, _}=Uri)
 when ?is_uri(T), is_list(Join) ->
   X = iolist_to_binary([[$/, scalar:s(X)] || X <- Join]),
   case uri:path(Uri) of
      undefined -> 
         uri:path(X, Uri);
      Path      ->
         uri:path(<<Path/binary, X/binary>>, Uri)
   end.


%%
%%
-spec q(uri()) -> [{binary(), binary()}].
-spec q(any(), uri()) -> uri().

q({uri, _, #uval{q=undefined}}) ->
   undefined;
q({uri, _, U}) ->
   [get_qelement(?COMPARE, X) || X <- binary:split(U#uval.q, <<$&>>, [global])]. 

get_qelement([], X)  ->
   unescape(X);

get_qelement([RegEx|T], X)
 when RegEx =/= <<$=>> ->
   case binary:match(X, RegEx) of
      nomatch -> 
         get_qelement(T, X);
      _       ->
         [Key, Val] = binary:split(X, RegEx),
         {binary_to_atom(RegEx, utf8), unescape(Key), unescape(Val)}
   end;

get_qelement([RegEx|T], X) ->
   case binary:match(X, RegEx) of
      nomatch -> 
         get_qelement(T, X);
      _       ->
         [Key, Val] = binary:split(X, RegEx),
         {unescape(Key), unescape(Val)}
   end.

q(undefined, {uri, S, U}) ->
   {uri, S, U#uval{q = undefined}};
q(Val, {uri, S, U})
 when is_binary(Val) ->
   {uri, S, U#uval{q = escape(Val)}};
q(Val, {uri, S, U})
 when is_list(Val) ->
   [H | T] = [set_qelement(X) || X <- Val],
   Query   = iolist_to_binary([H] ++ [[$&, X] || X <- T]),
   {uri, S, U#uval{q = Query}}.

set_qelement({Cmd, Key, Val}) ->
   <<(escape(scalar:s(Key)))/binary, (scalar:s(Cmd))/binary, (escape(scalar:s(Val)))/binary>>;
set_qelement({Key, Val}) ->
   <<(escape(scalar:s(Key)))/binary, $=, (escape(scalar:s(Val)))/binary>>;
set_qelement(Key) ->
   escape(scalar:s(Key)).



%%
%%
-spec anchor(uri()) -> binary().
-spec anchor(any(), uri()) -> uri().

anchor({uri, _, #uval{anchor=undefined}}) ->
   undefined;
anchor({uri, _, U}) ->
   unescape(U#uval.anchor).

anchor(undefined, {uri, S, U}) ->
   {uri, S, U#uval{anchor = undefined}};
anchor(Val, {uri, S, U}) ->
   {uri, S, U#uval{anchor = escape(scalar:s(Val))}}.

%%
%% suburi is /path?query#anchor
-spec suburi(uri()) -> binary().
-spec suburi(any(), uri()) -> binary().

suburi({uri, _, U}=Uri) ->
   suburi(uri:path(Uri), tosp(U#uval.q, $?), tosp(U#uval.anchor, $#)).

suburi(undefined, <<>>, <<>>) ->
   undefined;
suburi(undefined, Query, Anchor) ->
   <<$/, Query/binary, Anchor/binary>>;
suburi(Path, Query, Anchor) ->
   <<Path/binary, Query/binary, Anchor/binary>>.

suburi(undefined, Uri) ->
   uri:anchor(undefined,
      uri:q(undefined,
         uri:path(undefined, Uri)
      )
   );
   
suburi(Val, Uri) ->
   {Path, Query, Anchor} = parser_heir_query_anchor(Val),
   uri:anchor(Anchor,
      uri:q(Query,
         uri:path(Path, Uri)
      )
   ).


%%
%% compatibility wrapper for uri getter interface
-spec get(atom(), uri()) -> any().

get(Item,     Uri) 
 when is_binary(Uri) orelse is_list(Uri) -> 
   uri:get(Item, new(Uri));
get(schema,   Uri) -> uri:schema(Uri); 
get(userinfo, Uri) -> uri:userinfo(Uri);
get(host,     Uri) -> uri:host(Uri);
get(port,     Uri) -> uri:port(Uri);
get(authority,Uri) -> uri:authority(Uri); 
get(path,     Uri) -> uri:path(Uri); 
get(segments, Uri) -> uri:segments(Uri);
get(q,        Uri) -> uri:q(Uri); 
get(fragment, Uri) -> uri:anchor(Uri);
get(anchor,   Uri) -> uri:anchor(Uri);
get(suburi,   Uri) -> uri:suburi(Uri).

%%
%% compatibility wrapper for uri setter interface
set(Item,      Val, Uri)
 when is_binary(Uri) orelse is_list(Uri) -> 
   uri:set(Item, Val, new(Uri));
set(schema,    Val, Uri) -> uri:schema(Val, Uri);
set(userinfo,  Val, Uri) -> uri:userinfo(Val, Uri); 
set(host,      Val, Uri) -> uri:host(Val, Uri); 
set(port,      Val, Uri) -> uri:port(Val, Uri); 
set(authority, Val, Uri) -> uri:authority(Val, Uri); 
set(path,      Val, Uri) -> uri:path(Val, Uri); 
set(segments,  Val, Uri) -> uri:segments(Val, Uri);
set(q,         Val, Uri) -> uri:q(Val, Uri); 
set(fragment,  Val, Uri) -> uri:anchor(Val, Uri);
set(anchor,    Val, Uri) -> uri:anchor(Val, Uri);
set(suburi,    Val, Uri) -> uri:suburi(Val, Uri).


%%
%% helper function to read key value from query
-spec q(any(), any(), uri()) -> uri().

q(_Key, Default, {uri, _, #uval{q=undefined}}) ->
   Default;
q(Key, Default, Uri) ->
   Qlist = uri:q(Uri),
   Qkey  = scalar:s(Key),
   case lists:keyfind(Qkey, 1, Qlist) of
      false    -> 
         case lists:member(Qkey, Qlist) of
            true  -> true;
            false -> Default
         end;
      {_, Val} -> 
         Val
   end.

%%
%% check([Elements], Uri) -> ok 
%%
%% validates that URI components is defined, fails badarg otherwise
%% @todo: rename assert
check([], {uri, _,_} = Uri) ->
   Uri;
check([{Key, Val} | T], {uri, _,_} = Uri) ->
   case uri:get(Key, Uri) of
      Val -> check(T, Uri);
      _   -> throw({badarg, Key, Val})
   end;
check([Key | T], {uri, _,_} = Uri) ->
   case uri:get(Key, Uri) of
      <<>>      -> throw({badarg, Key});
      undefined -> throw({badarg, Key});
      _         -> check(T, Uri)
   end;
check(List, Uri) ->
   check(List, new(Uri)).  

%%%------------------------------------------------------------------
%%%
%%% convert
%%%
%%%------------------------------------------------------------------

%%
%%
c(Uri) ->
   binary_to_list(uri:s(Uri)).

%%
%%
s(undefined)   ->
   undefined;
s({uri, S, U}) ->
   Schema= schema_to_s(S), 
   User  = toss(U#uval.user,   $@),
   Host  = tos(U#uval.host),
   Port  = tosp(U#uval.port,   $:),
   Path  = tos(U#uval.path),
   Query = tosp(U#uval.q,      $?),
   Anchor= tosp(U#uval.anchor, $#),
   Auth  = if
      User =/= <<>> orelse Port =/= <<>> orelse Host =/= <<>> -> 
         case S of
            undefined -> <<>>;
            _         -> <<"//">>
         end;
      true -> 
         case Path of
            <<$/, _/binary>> ->
               case S of
                  undefined -> <<>>;
                  _         -> <<"//">>
               end;
            _ -> 
               <<>>
         end
   end,
   <<Schema/binary, Auth/binary, User/binary, Host/binary, Port/binary, Path/binary, Query/binary, Anchor/binary>>;
s({urn, S, P}) ->
  Schema= schema_to_s(S), 
  Path  = tos(P),
  <<"urn:", Schema/binary, Path/binary>>.

%% to string
tos(undefined) ->
   <<>>;
tos(Val) ->
   scalar:s(Val).

%% to string with prefix
tosp(undefined, _) ->
   <<>>;
tosp(Val,     Pfx) ->
   <<Pfx, (scalar:s(Val))/binary>>.

%% to string with suffix
toss(undefined, _) ->
   <<>>;
toss(Val,     Sfx) ->
   <<(scalar:s(Val))/binary, Sfx>>.

%%
schema_to_s(undefined) ->
   <<>>;
schema_to_s(Val)
 when not is_list(Val) ->
   <<(scalar:s(Val))/binary, $:>>;
schema_to_s([H|T]) ->
   Schema = iolist_to_binary([scalar:s(H)] ++ [[$+, scalar:s(X)] || X <- T]),
   <<Schema/binary, $:>>.   

%%
%% to urn
%% replaces urn prefix with short name and return corresponding urn schema
-spec urn(uri(), any()) -> binary().

urn({uri, _, _}=Uri, Prefixes)
 when is_list(Prefixes) ->
   urn(uri:s(Uri), Prefixes);

urn({urn, _, _}=Urn, _Prefixes) ->
   Urn;

urn(<<"urn:", _/binary>>=Urn, _Prefixes) ->
   Urn;

urn(Uri, [{Prefix, Head} | Prefixes])
 when is_binary(Uri) ->
   Len = byte_size(Head),
   case Uri of
      <<Head:Len/binary, Tail/binary>> ->
         uri:s({urn, Prefix, Tail});
      _ ->
         urn(Uri, Prefixes)
   end;

urn(Uri, []) ->
   Uri.



%%%------------------------------------------------------------------
%%%
%%% uri escape/unescape
%%%
%%%------------------------------------------------------------------

%%
%% escape
escape(X) when is_binary(X) ->
   escape(X, <<>>);
escape(X) ->
   escape(scalar:s(X)).


escape(<<H:8, Bin/binary>>, Acc) when H >= $a, H =< $z ->
   escape(Bin, <<Acc/binary, H>>);
escape(<<H:8, Bin/binary>>, Acc) when H >= $A, H =< $Z ->
   escape(Bin, <<Acc/binary, H>>);
escape(<<H:8, Bin/binary>>, Acc) when H >= $0, H =< $9 ->
   escape(Bin, <<Acc/binary, H>>);

escape(<<$ , Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $%, $2, $0>>);
   %escape(Bin, <<Acc/binary, $+>>);

%% unreserved "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"
escape(<<$-, Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $->>);
escape(<<$_, Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $_>>);
escape(<<$., Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $.>>);
escape(<<$!, Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $!>>);
escape(<<$~, Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $~>>);
escape(<<$*, Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $*>>);
escape(<<$', Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $'>>);
escape(<<$(, Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $(>>);
escape(<<$), Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $)>>);

escape(<<H:8, Bin/binary>>, Acc) when H =< 16#7f ->
   escape(Bin, <<Acc/binary, $%, (escape_byte(H))/binary>>);

escape(<<H:8, Bin/binary>>, Acc) when H  > 16#7f ->
   escape(Bin, 
      <<Acc/binary,
      $%, (escape_byte((H bsr      6) + 16#c0))/binary, 
      $%, (escape_byte((H band 16#3f) + 16#80))/binary>>
   );

escape(<<>>, Acc) ->
   Acc.

escape_byte(H) ->
   <<(hex(H div 16)), (hex(H rem 16))>>.

hex(H) when H <  10 ->
   $0 + H;
hex(H) when H >= 10 ->
   $A + (H - 10).

%%
%% unescape
unescape(Bin) ->
   decode(unescape(Bin, <<>>), <<>>).

unescape(<<$%, H:8, L:8, Bin/binary>>, Acc) ->
   unescape(Bin, <<Acc/binary, (unescape_byte(H, L))>>);
unescape(<<H:8, Bin/binary>>, Acc) ->
   unescape(Bin, <<Acc/binary, H>>);
unescape(<<>>, Acc) ->
   Acc.

unescape_byte(H, L) ->
   int(H) * 16 + int(L).

int(C) when $0 =< C, C =< $9 ->
    C - $0;
int(C) when $A =< C, C =< $F ->
    C - $A + 10;
int(C) when $a =< C, C =< $f ->
    C - $a + 10.

%%
%% decode utf8 (TODO: utf8 for binaries)
decode(<<H:8, L:8, Rest/binary>>, Acc) when H >= 16#C0 ->
   decode(Rest, <<Acc/binary, ((H bsl 6) + (L band 16#3f))>>);
decode(<<H:8, Rest/binary>>, Acc) ->
   decode(Rest, <<Acc/binary, H>>);
decode(<<>>, Acc) ->
   Acc.

%%
%% utility function to convert ip address to network
-spec aton(any()) -> integer().

aton({A, B, C, D}) ->
   (A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D;
aton(IP)
 when is_list(IP) ->
   {ok, Addr} = inet_parse:address(IP),
   aton(Addr);
aton(IP)
 when is_binary(IP) ->
   aton(scalar:c(IP)).

%%
%% utility function to discover uri handler
-spec registry(atom(), _) -> atom().

registry(App, {uri, _, _} = Uri) ->
   (opts:val(registry, App)):lookup(App, uri:schema(Uri));

registry(App, Uri) ->
   registry(App, uri:new(Uri)).

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%      URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
parse_uri(Uri0) ->
   {Scheme, Uri1} = parse_schema(Uri0),
   {Heir, Query, Anchor} = parser_heir_query_anchor(Uri1),
   % heir
   {Auth0, Path} = case Heir of
      <<"//", H/binary>> -> 
         case suffix(H, <<$/>>) of
            {A, undefined} -> {A, undefined};
            {A,    P} -> {A, <<$/, P/binary>>}
         end;
      _                  -> {undefined, Heir}
   end,
   {User, Host0} = prefix(Auth0, <<$@>>),
   {Host,  Pbin} = case Host0 of
      <<$[, IP6/binary>> ->
         [Addr, <<$:, Pport/binary>>] = binary:split(IP6, <<$]>>),
         {Addr, Pport};
      _ ->
         suffix(Host0, <<$:>>)
   end,
   % parse port
   Port = case Pbin of
      undefined ->  undefined;
      _    ->  list_to_integer(binary_to_list(Pbin))
   end,
   {Scheme, #uval{user=User, host=Host, port=Port, path=Path, q=Query, anchor=Anchor}}.

%%
parse_schema(<<$/, _/binary>>=Uri) ->
   {undefined, Uri};
parse_schema(Uri) ->
   {Scheme, Suffix} = prefix(Uri, <<$:>>),
   {schema_to_uri(Scheme), Suffix}.

%% convert binary schema to uri format
schema_to_uri(undefined) ->
   undefined;
schema_to_uri(Scheme) ->
   case binary:split(Scheme, <<$+>>, [global]) of
      [S] ->  binary_to_atom(S, utf8);
       S  -> [binary_to_atom(X, utf8) || X <- S]
   end.

%%
parser_heir_query_anchor(Uri) ->
   case suffix(Uri, <<$?>>) of
      {Prefix, undefined} ->
         {Heir, Anchor}  = suffix(Prefix,   <<$#>>),
         {Heir, undefined, Anchor};
      {Heir, Suffix}    ->
         {Query, Anchor} = suffix(Suffix, <<$#>>),
         {Heir, Query, Anchor}
   end.


%%
%% split Uri substring at token T, 
%% return empty suffix if T is not found
suffix(undefined, _) ->
   {undefined, undefined};    
suffix(Uri, T) ->
   case binary:split(Uri, T) of
      [Token, Rest] -> {Token, Rest};
      _             -> {Uri,   undefined}
   end.

%%
%% split Uri substring at token T,
%% return empty prefix if T is not found
prefix(undefined, _) ->
   {undefined, undefined};
prefix(Uri, T) ->
   case binary:split(Uri, T) of
      [Token, Rest] -> {Token, Rest};
      _             -> {undefined,  Uri}
   end.   


%%
%% maps schema to default ports
schema_to_port([S| _], P)         -> schema_to_port(S, P);
schema_to_port(tcp,    undefined) -> 80;   % custom schema for tcp sensors
schema_to_port(http,   undefined) -> 80;
schema_to_port(ws,     undefined) -> 80;
schema_to_port(ssl,    undefined) -> 443;  % custom schema for ssl sensors 
schema_to_port(https,  undefined) -> 443;
schema_to_port(wss,    undefined) -> 443;
schema_to_port(mysql,  undefined) -> 3306;
schema_to_port(_,      undefined) -> undefined;
schema_to_port(_,           Port) -> scalar:i(Port).
