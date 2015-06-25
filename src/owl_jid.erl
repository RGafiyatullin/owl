-module (owl_jid).
-export ([
		nil/0, is_jid/1,

		new/3, new/2, new/1,
		node/1, host/1, resource/1,
		is_bare/1, bare/1, set_resource/2,

		j2b/1, b2j/1
	]).
-export_type ([
		jid/0
	]).

-type jid_part() :: binary().

-define( nil, <<>> ).
-record( jid, {
		node :: jid_part(),
		host :: jid_part(),
		resource :: jid_part()
	} ).

-type jid() :: #jid{}.

-spec nil() -> jid_part().
-spec is_jid( jid() | term() ) -> boolean().
-spec is_bare( jid() ) -> boolean().
-spec bare( jid() ) -> jid().
-spec set_resource( jid(), jid_part() ) -> jid().
-spec new( N :: jid_part(), H :: jid_part(), R :: jid_part() ) -> jid().
-spec new( N :: jid_part(), H :: jid_part() ) -> jid().
-spec new( H :: jid_part() ) -> jid().

-spec node( jid() ) -> jid_part().
-spec host( jid() ) -> jid_part().
-spec resource( jid() ) -> jid_part().

-spec j2b( jid() ) -> binary().
-spec b2j( binary() ) -> jid().

nil() -> ?nil.

is_jid( #jid{} ) -> true;
is_jid( _ ) -> false.

is_bare( J ) -> resource( J ) == nil().

new( Node, Host, Resource ) ->
	#jid{
		node = node_prep(Node),
		host = host_prep(Host),
		resource = res_prep(Resource)
	}.
new( Node, Host ) -> new( Node, Host, nil() ).
new( Host ) -> new( nil(), Host, nil() ).

bare( J ) -> set_resource( J, nil() ).
set_resource( J, R ) -> J #jid{ resource = res_prep( R ) }.


node( #jid{ node = N } ) -> N.
host( #jid{ host = H } ) -> H.
resource( #jid{ resource = R } ) -> R.


j2b( #jid{ node = N, host = H, resource = R } ) ->
	case {N, H, R, nil()} of
		{Nil, NotNil, Nil, Nil} when NotNil /= Nil -> H;
		{Nil, _, NotNil, Nil} when NotNil /= Nil -> <<H/binary, $/, R/binary>>;
		{NotNil, _, Nil, Nil} when NotNil /= Nil -> <<N/binary, $@, H/binary>>;
		{_, _, _, _} -> <<N/binary, $@, H/binary, $/, R/binary>>
	end.

b2j( JidBin ) ->
	{Node, HostAndRes} =
		case binary:split(JidBin, <<$@>>) of
			[ NodePart, NoNodePart ] -> {NodePart, NoNodePart};
			[ NoNodePart ] -> {nil(), NoNodePart}
		end,
	{Host, Resource} =
		case binary:split(HostAndRes, <<$/>>) of
			[ HostPart, ResPart ] -> {HostPart, ResPart};
			[ HostPart ] -> {HostPart, nil()}
		end,
	#jid{ node = node_prep(Node), host = host_prep(Host), resource = res_prep(Resource) }.



%% The following four are the stubs currently
node_prep( B ) when is_binary( B ) -> B.
host_prep( B ) when is_binary( B ) -> to_lower( B ).
res_prep( B ) when is_binary( B ) -> B.
to_lower( B ) when is_binary( B ) -> B.
