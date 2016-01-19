

-module (owl_xmpp_jid).

-export([nil/0]).
-export([jid2bin/1, bin2jid/1, j2b/1, b2j/1]).
-export([jid/3, jid/2, jid/1, new/3, new/2, new/1]).
-export([node/1, host/1, resource/1]).
-export([to_bare/1, to_host/1, set_resource/2]).
-export([is_jid/1, is_full/1, is_bare/1, is_host/1]).

-define(jid_nil, <<>>).
-record(jid, {
	node = ?jid_nil :: jid_node(),
	host = ?jid_nil :: jid_host(),
	resource = ?jid_nil :: jid_resource()
}).

-type jid_node() :: binary().
-type jid_host() :: binary().
-type jid_resource() :: binary().
-type jid() :: #jid{}.
-type jid_part_raw() :: binary() | string() | ?jid_nil.

-export_type([jid_node/0, jid_host/0, jid_resource/0, jid/0]).


%% Interface


-spec nil() ->
	<<>>.

nil() ->
	?jid_nil.


-spec new(
	Node :: jid_part_raw(),
	Host :: jid_part_raw(),
	Resource :: jid_part_raw()
) ->
	Ret :: jid().

new(Node, Host, Resource) ->
	jid(Node, Host, Resource).


-spec new(
	Node :: jid_node(),
	Host :: jid_host()
) ->
	Ret :: jid().

new(Node, Host) ->
	jid(Node, Host).


-spec new(Host :: jid_host()) ->
	Ret :: jid().

new(Host) ->
	jid(Host).


-spec jid(
	Node :: jid_part_raw(),
	Host :: jid_part_raw(),
	Resource :: jid_part_raw()
) ->
	Ret :: jid().

jid(Node, Host, Resource)
	when is_binary(Node)
	andalso is_binary(Host)
	andalso is_binary(Resource)
->
	#jid{
		node = nodeprep(Node),
		host = hostprep(Host),
		resource = resourceprep(Resource)
	};

jid(Node, Host, Resource) when is_list(Node) ->
	jid(utfl_to_bin(Node), Host, Resource);

jid(Node, Host, Resource) when is_list(Host) ->
	jid(Node, utfl_to_bin(Host), Resource);

jid(Node, Host, Resource) when is_list(Resource) ->
	jid(Node, Host, utfl_to_bin(Resource));

jid(Node, Host, Resource) ->
	error({badarg, jid, {Node,Host,Resource}}).


-spec jid(
	Node :: jid_node(),
	Host :: jid_host()
) ->
	Ret :: jid().

jid(Node, Host) ->
	jid(Node, Host, ?jid_nil).


-spec jid(Host :: jid_host()) ->
	Ret :: jid().

jid(Host) ->
	jid(?jid_nil, Host, ?jid_nil).


-spec j2b(Jid :: jid()) ->
	Ret :: binary().

j2b(Jid) ->
	jid2bin(Jid).


-spec jid2bin(#jid{}) ->
	Ret :: binary().

jid2bin(#jid{node = ?jid_nil, host = Host, resource = ?jid_nil}) ->
	<<Host/binary>>;

jid2bin(#jid{node = ?jid_nil, host = Host, resource = Resource}) ->
	<<Host/binary, $/, Resource/binary>>;

jid2bin(#jid{node = Node, host = Host, resource = ?jid_nil}) ->
	<<Node/binary, $@, Host/binary>>;

jid2bin(#jid{node = Node, host = Host, resource = Resource}) ->
	<<Node/binary, $@, Host/binary, $/, Resource/binary>>.


-spec b2j(Bin :: binary()) ->
	Ret :: jid().

b2j(Bin) ->
	bin2jid(Bin).


-spec bin2jid(Bin :: binary()) ->
	Ret :: jid().

bin2jid(Bin) when is_binary(Bin) ->
	parse_jid(Bin);

bin2jid(List) when is_list(List) ->
	parse_jid(utfl_to_bin(List)).


-spec to_bare(Jid :: jid()) ->
	Ret :: jid().

to_bare(Jid = #jid{resource = ?jid_nil}) ->
	Jid;

to_bare(Jid) ->
	Jid#jid{resource = ?jid_nil}.


-spec to_host(Jid :: jid()) ->
	Ret :: jid().

to_host(Jid = #jid{resource = ?jid_nil, node = ?jid_nil}) ->
	Jid;

to_host(Jid) ->
	Jid#jid{resource = ?jid_nil, node = ?jid_nil}.


-spec set_resource(Jid :: jid(), Resource :: jid_resource()) ->
	Ret :: jid().

set_resource(Jid = #jid{resource = Resource}, Resource) ->
	Jid;

set_resource(Jid, Resource) ->
	Jid#jid{resource = resourceprep(Resource)}.


-spec is_jid(Jid :: jid()) ->
	Ret :: boolean().

is_jid(#jid{}) ->
	true;

is_jid(_Jid) ->
	false.


-spec is_full(Jid :: jid()) ->
	Ret :: boolean().

is_full(#jid{node = Node, host = Host, resource = Resource}) when
?jid_nil =/= Node,
?jid_nil =/= Host,
?jid_nil =/= Resource ->
	true;

is_full(_Jid) ->
	false.


-spec is_bare(Jid :: jid()) ->
	Ret :: boolean().

is_bare(#jid{resource = ?jid_nil}) ->
	true;

is_bare(_Jid) ->
	false.


-spec is_host(Jid :: jid()) ->
	Ret :: boolean().

is_host(#jid{node = ?jid_nil, resource = ?jid_nil}) ->
	true;

is_host(_Jid) ->
	false.


-spec node(Jid :: jid()) ->
	Ret :: jid_node().

node(#jid{node = Node}) ->
	Node.


-spec host(Jid :: jid()) ->
	Ret :: jid_host().

host(#jid{host = Host}) ->
	Host.


-spec resource(Jid :: jid()) ->
	Ret :: jid_resource().

resource(#jid{resource = Resource}) ->
	Resource.


%% Internals


-spec nodeprep(Node :: jid_node()) ->
	Ret :: jid_node().

nodeprep(Node) ->
	chk(to_lower(Node), nodeprep).


-spec hostprep(Host :: jid_host()) ->
	Ret :: jid_host().

hostprep(Host) ->
	chk(to_lower(Host), hostprep).


-spec resourceprep(Resource :: jid_resource()) ->
	Ret :: jid_resource().

resourceprep(Resource) ->
	chk(Resource, resourceprep).


-spec chk(Bin :: jid_node() | jid_host() | jid_resource(), Type :: nodeprep | hostprep | resourceprep) ->
	Ret :: jid_node() | jid_host() | jid_resource().

chk(Bin, resourceprep) -> Bin;
chk(Bin, nodeprep) ->
	case binary:match(Bin, [<<$/>>]) of
		nomatch -> Bin;
		_ -> error({badarg, nodeprep, Bin})
	end;
chk(Bin, hostprep) ->
	case binary:match(Bin, [<<$@>>, <<$/>>]) of
		nomatch -> Bin;
		_ -> error({badarg, hostprep, Bin})
	end.


-spec parse_jid(binary()) ->
	jid().

parse_jid(StrJid) when is_binary(StrJid) ->
	{StrJidBare, Resource} =
		case binary:split(StrJid, [<<$/>>]) of
			[SinglePart] -> {SinglePart, nil()};
			[Left, Right] -> {Left, Right}
		end,
	{Node, Host} = binary_split_reverse(StrJidBare, $@),
	jid(Node, Host, Resource).

binary_split_reverse(Bin, Ch) ->
	BinSize = size(Bin),
	case binary_find_reverse_loop(BinSize - 1, Bin, Ch) of
		undefined -> {nil(), Bin};
		Pos ->
			<<Left:Pos/binary, _, Right/binary>> = Bin,
			{Left, Right}
	end.


binary_find_reverse_loop(0, _, _) -> undefined;
binary_find_reverse_loop(Pos, Bin, Ch) ->
	case binary:at(Bin, Pos) of
		Ch -> Pos;
		_ -> binary_find_reverse_loop(Pos - 1, Bin, Ch)
	end.

-spec to_lower(Bin :: binary()) ->
	Ret :: binary().

to_lower(Bin) when is_binary(Bin) ->
	unistring:to_lower(Bin).


-spec utfl_to_bin(List :: [integer()]) ->
	binary().

utfl_to_bin(List) ->
	unicode:characters_to_binary(List, utf8, utf8).
