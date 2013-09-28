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
%%   * espace works badly when URI is copied (proper escaping)
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
   % uri template function
   template/1, 
   % helper functions
   to_binary/1,
   s/1,
   c/1,
   unescape/1, 
   escape/1,
   % deprecated
   add/3,
   match/2
]).
%    check/2]).
% -export([add/3]).
% -export([match/2]).
% -export([]).

-export_type([uri/0]).

%% 
-type(uri()    :: {uri, schema(), any()}).
-type(schema() :: atom() | [atom()]).

%% internal uri structures
-record(uval, {
   user  = undefined :: binary(),
   host  = undefined :: binary(),
   port  = undefined :: integer(),
   path  = undefined :: binary(),
   q     = undefined :: binary(),
   anchor= undefined :: binary() 
}).

%% @depricated
-define(USER,  1).
-define(HOST,  2).
-define(PORT,  3).
-define(PATH,  4).
-define(QUERY, 5).
-define(FRAG,  6).

%% query operation
-define(COMPARE, [<<$=, $<>>, <<$>, $=>>, <<$>>>, <<$<>>, <<$=>>]).

%%
%% parses URI into tuple, fails with badarg if invalid URI
-spec(new/0 :: () -> uri()).
-spec(new/1 :: (list() | binary() | atom() | uri()) -> uri()).

new() ->
   new(undefined).

new(Uri)
 when is_binary(Uri) ->
   {Schema, UVal} = parse_uri(Uri),
   {uri, Schema, UVal};

new(Uri)
 when is_list(Uri) ->
   new(list_to_binary(Uri));

new(Schema)
 when is_atom(Schema) ->
   {uri, Schema, #uval{}};

new({uri, _, _} = Uri) ->
   Uri.

%%%------------------------------------------------------------------
%%%
%%% setter / getters
%%%
%%%------------------------------------------------------------------   

%%
%% uri schema
-spec(schema/1 :: (uri()) -> schema()).
-spec(schema/2 :: (schema(), uri()) -> uri()).

schema({uri, S, _}) ->
   S.

schema(Val, {uri, _, U})
 when is_atom(Val) orelse is_list(Val) ->
   {uri, Val, U}.

%%
%% uri user info
-spec(userinfo/1 :: (uri()) -> {binary(), binary()} | binary()).
-spec(userinfo/2 :: ({any(), any()} | any(), uri()) -> uri()).

userinfo({uri, _, #uval{user=undefined}}) ->
   undefined;
userinfo({uri, _, U}) ->
   case binary:split(U#uval.user, <<$:>>) of
      [User, Pass] -> {unescape(User), unescape(Pass)};
      [Info]       -> unescape(Info)
   end.

userinfo(undefined, {uri, S, U}) ->
   {uri, S, U#uval{user = undefined}};
userinfo({User0, Pass0}, {uri, S, U}) ->
   User = escape(scalar:s(User0)),
   Pass = escape(scalar:s(Pass0)),
   {uri, S, U#uval{user = <<User/binary, $:, Pass/binary>>}};
userinfo(Val, {uri, S, U}) ->
   {uri, S, U#uval{user = escape(scalar:s(Val))}}.

%%
%%
-spec(host/1 :: (uri()) -> binary()).
-spec(host/2 :: (any(), uri()) -> uri()).

host({uri, _, U}) ->
   U#uval.host.

host(undefined, {uri, S, U}) ->
   {uri, S, U#uval{host = undefined}};
host(Val, {uri, S, U}) ->
   {uri, S, U#uval{host = scalar:s(Val)}}.

%%
%%
-spec(port/1 :: (uri()) -> integer()).
-spec(port/2 :: (any(), uri()) -> uri()).

port({uri, S, U}) ->
   schema_to_port(S, U#uval.port).

port(undefined, {uri, S, U}) ->
   {uri, S, U#uval{port = undefined}};
port(Val, {uri, S, U}) ->
   {uri, S, U#uval{port = scalar:i(Val)}}.

%%
%%
-spec(authority/1 :: (uri()) -> {binary(), integer()}).
-spec(authority/2 :: ({any(), any()} | any(), uri()) -> uri()).

authority({uri, _, _}=Uri) ->
   {uri:host(Uri), uri:port(Uri)}.

authority(undefined, Uri) ->
   uri:port(undefined, uri:host(undefined, Uri));

authority({Host, Port}, {uri, _, _}=Uri) ->
   uri:port(scalar:i(Port), uri:host(Host, Uri));

authority(Val, {uri, _, _}=Uri) ->
   case binary:split(scalar:s(Val), <<$:>>) of
      [Host, Port] -> uri:authority({Host, Port}, Uri);
      [Host]       -> uri:host(Host, Uri)
   end.

%%
%%
-spec(path/1 :: (uri()) -> binary()).
-spec(path/2 :: (any(), uri()) -> uri()).

path({uri, _, #uval{path=undefined}}) ->
   undefined;
path({uri, _, U}) ->
   case unescape(U#uval.path) of
      <<>> -> <<$/>>;
      V    -> V
   end.

path(undefined, {uri, S, U}) ->
   {uri, S, U#uval{path = undefined}};   
path(Val, {uri, S, U}) ->
   % @todo: do we need to escape path
   {uri, S, U#uval{path = scalar:s(Val)}}.

%%
%%
-spec(segments/1 :: (uri()) -> [binary()]).
-spec(segments/2 :: (any(), uri()) -> uri()).

segments({uri, _, #uval{path=undefined}}) ->
   undefined;
segments({uri, _, U}) ->
   case binary:split(U#uval.path, <<"/">>, [global, trim]) of
      []         -> [];
      [_ | Segs] -> Segs
   end.

segments(undefined, Uri) ->
   uri:path(undefined, Uri);

segments(Val, Uri)
 when is_list(Val) ->
   uri:path(iolist_to_binary([[$/, escape(X)] || X <- Val]), Uri);

segments(Val, Uri)
 when is_tuple(Val) ->
   uri:segments(tuple_to_list(Val), Uri).

%%
%%
-spec(q/1 :: (uri()) -> [{binary(), binary()}]).
-spec(q/2 :: (any(), uri()) -> uri()).

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
         {binary_to_atom(RegEx, utf8), unescape(Key), scalar:decode(unescape(Val))}
   end;

get_qelement([RegEx|T], X) ->
   case binary:match(X, RegEx) of
      nomatch -> 
         get_qelement(T, X);
      _       ->
         [Key, Val] = binary:split(X, RegEx),
         {unescape(Key), scalar:decode(unescape(Val))}
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
-spec(anchor/1 :: (uri()) -> binary()).
-spec(anchor/2 :: (any(), uri()) -> uri()).

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
-spec(suburi/1 :: (uri()) -> binary()).
-spec(suburi/2 :: (any(), uri()) -> binary()).

suburi({uri, _, U}=Uri) ->
   <<(uri:path(Uri))/binary, (tosp(U#uval.q, $?))/binary, (tosp(U#uval.anchor, $#))/binary>>.

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
-spec(get/2 :: (atom(), uri()) -> any()).

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
-spec(q/3 :: (any(), any(), uri()) -> uri()).

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
%% convert uri to binary
%% @depricated
to_binary(Uri) ->
   uri:s(Uri).

%%
%%
c(Uri) ->
   binary_to_list(uri:s(Uri)).

%%
%%
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
         <<"//">>;
      true -> 
         <<>>
   end,
   <<Schema/binary, Auth/binary, User/binary, Host/binary, Port/binary, Path/binary, Query/binary, Anchor/binary>>.

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


%%%------------------------------------------------------------------
%%%
%%% uri escape/unescape
%%%
%%%------------------------------------------------------------------

%%
%% escape
escape(X) when is_binary(X) ->
   escape(X, <<>>);
escape(X) when is_float(X)  ->
   escape(list_to_binary(format:decimal(X)));
escape(X) ->
   escape(list_to_binary(format:scalar(X))).


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

%%%------------------------------------------------------------------
%%%
%%% template
%%%
%%%------------------------------------------------------------------   

%%
%% make uri template
template(TUri)
 when is_list(TUri) orelse is_binary(TUri) ->
   Uri = uri:new(TUri),
   {turi, uri:schema(Uri), {uri:host(Uri), uri:port(Uri), uri:segments(Uri)}};

template({turi, _, _}=TUri) ->
   TUri.

%%
%% match uri to template
match({uri, _, _}=Uri, {turi, _, _}=TUri) ->
   match_segments(Uri, TUri,
      match_port(Uri, TUri,
         match_host(Uri, TUri,
            match_schema(Uri, TUri, true)
         )
      )
   );

match(Uri, TUri) ->
   match(uri:new(Uri), uri:template(TUri)).

%%
match_schema({uri, _, _}=Uri, {turi, Schema, _}, Acc) ->
   match_acc(Acc, match_token(uri:schema(Uri), Schema)).

%%
match_host({uri, _, _}=Uri, {turi, _, {Host, _, _}}, Acc) ->
   match_acc(Acc, match_token(uri:host(Uri), Host)).

%%
match_port({uri, _, _}, {turi, _, {_, undefined, _}}, Acc) ->
   Acc;
match_port({uri, _, _}=Uri, {turi, _, {_, Port, _}}, Acc) ->
   match_acc(Acc, match_token(uri:port(Uri), Port)).

%%
match_segments({uri, _, _}=Uri, {turi, _, {_, _, Segments}}, Acc) ->
   match_segments(uri:segments(Uri), Segments, Acc);

match_segments(_, [<<$*>>], Acc) ->
   match_acc(Acc, true);

match_segments([A | Atail], [B | Btail], Acc) ->
   match_segments(Atail, Btail, match_acc(Acc, match_token(A, B)));

match_segments([], [], Acc) ->
   Acc;

match_segments(undefined, undefined, Acc) ->
   Acc;

match_segments(_, _, _) ->
   false.

%%   
%% match individual token
match_token(_Token, Lit)
 when Lit =:= <<$*>> orelse Lit =:= '*' ->
   true;

match_token(undefined, undefined) ->
   true;

match_token(undefined, _) ->
   false;

match_token(_, undefined) ->
   false;

match_token(_Token, Lit)
 when Lit =:= <<$_>> orelse Lit =:= '_' ->
   true;

match_token(Token,  <<$:, Key/binary>>) ->
   {Key, Token};

match_token(Token, Lit) ->
   scalar:c(Token) =:= scalar:c(Lit).

%%
%% accumulate match results
match_acc(false, _) ->
   false;
match_acc(_, false) ->
   false;
match_acc(true, {_, _}=Acc) ->
   [Acc];
match_acc(true, Acc) ->
   Acc;
match_acc(List, {_,_}=Acc) ->
   [Acc | List];
match_acc(List, _Acc) ->
   List.



%%%------------------------------------------------------------------
%%%
%%% deprecated
%%%
%%%------------------------------------------------------------------

%%
%% add(Item, V, Uri) -> NUri
%%
%% add path token to uri
add(path, V, {uri, _, _}=Uri) when is_binary(V) ->
   case uri:get(path, Uri) of
      X when X =:= <<$/>> orelse X =:= undefined -> 
         uri:set(path, <<$/, V/binary>>, Uri);
      Path   ->
         case binary:last(Path) of
            $/ -> 
               uri:set(path, <<Path/binary, V/binary>>, Uri);
            _  ->
               uri:set(path, <<Path/binary, $/, V/binary>>, Uri)
         end
   end;
add(Item, V, {uri, _, _}=Uri) when is_list(V) ->
   case io_lib:printable_unicode_list(V) of
      true  -> 
         add(Item, list_to_binary(V), Uri);
      false -> 
         lists:foldl(fun(X, Acc) -> uri:add(Item, X, Acc) end, Uri, V)
   end;
add(Item, V, {uri, _, _}=Uri) when is_tuple(V)->
   add(Item, tuple_to_list(V), Uri);
add(Item, V, {uri, _, _}=Uri) when is_atom(V) ->
   add(Item, atom_to_binary(V, utf8), Uri).

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
   {Host,  Pbin} = suffix(Host0, <<$:>>),
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
