-module (owl_xmpp_stanza).
-export ([
		make_id/1,
		id/1, set_id/2,
		to/1, set_to/2,
		from/1, set_from/2
	]).

make_id( [ FirstProp | Props ] ) ->
	iolist_to_binary( [
			integer_to_list( erlang:phash2( FirstProp ) )
			| [
					begin [$:, integer_to_list( erlang:phash2( Prop ) )] end
					|| Prop <- Props
				]
		]).

id( Xml ) ->
	exp_node_attrs:attr( <<"id">>, Xml ).

set_id( ID, Xml ) when ID == undefined orelse is_binary( ID ) ->
	exp_node_attrs:set_attr( <<"id">>, ID, Xml ).


from( Xml ) ->
	case exp_node_attrs:attr( <<"from">>, Xml ) of
		undefined -> undefined;
		BinJID ->
			owl_xmpp_jid:b2j( BinJID )
	end.


set_from(undefined, Xml) ->
	exp_node_attrs:set_attr(<<"from">>, undefined, Xml);

set_from( From, Xml ) ->
	FromBin = owl_xmpp_jid:j2b( From ),
	exp_node_attrs:set_attr( <<"from">>, FromBin, Xml ).


to( Xml ) ->
	case exp_node_attrs:attr( <<"to">>, Xml ) of
		undefined -> undefined;
		BinJID ->
			owl_xmpp_jid:b2j( BinJID )
	end.


set_to(undefined, Xml) ->
	exp_node_attrs:set_attr(<<"to">>, undefined, Xml);

set_to( To, Xml ) ->
	ToBin = owl_xmpp_jid:j2b( To ),
	exp_node_attrs:set_attr( <<"to">>, ToBin, Xml ).
