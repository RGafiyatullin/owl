-module (owl_stanza_sasl).
-export ([
		is_stanza_sasl/1,

		stanza_new/3,
		parse_stanza/1,

		feature_mechanisms_has_mech/2
	]).

-include_lib ("expellee/include/xml.hrl").
-include ("ns_xmpp_core.hrl").

-spec is_stanza_sasl( xml_element() ) -> boolean().
is_stanza_sasl( Xml ) ->
	exp_node:ns( Xml ) == ?ns_xmpp_sasl.

-spec stanza_new( xml_ncname(), [ xml_attribute() ], [ binary() ] ) -> xml_element().
stanza_new( NCN, Attrs, PayloadValues ) ->
	Children =
		case PayloadValues of
			[] -> [];
			[ _ | _ ] ->
				Packed = payload_pack_values( PayloadValues ),
				B64ed = base64:encode( iolist_to_binary(Packed) ),
				[ exp_text_node:cdata_new( B64ed ) ]
		end,
	exp_node:new( {?ns_xmpp_sasl, NCN}, Attrs, Children ).

-spec parse_stanza( xml_element() ) -> {ok, xml_ncname(), [xml_attribute()], [binary()]} | {error, term()}.
parse_stanza( Xml ) ->
	case exp_node:fqn( Xml ) of
		{?ns_xmpp_sasl, NCN} ->
			Attrs = exp_node_attrs:attrs( Xml ),
			FlatText = exp_text:text_flat( Xml ),
			case maybe_b64_decode( FlatText ) of
				undefined ->
					{ok, NCN, Attrs, FlatText};
				Packed ->
					PayloadValues = binary:split( Packed, [<<0>>], [global] ),
					{ok, NCN, Attrs, PayloadValues}
			end;

		{_NS, _} ->
			{error, {badarg, non_sasl_ns}}
	end.

-spec feature_mechanisms_has_mech( binary(), xml_element() ) -> boolean().
feature_mechanisms_has_mech( MechNameSought, SaslFeature ) ->
	{?ns_xmpp_sasl, <<"mechanisms">>} = exp_node:fqn( SaslFeature ),
	lists:any(
		fun( MaybeMech ) ->
			{?ns_xmpp_sasl, <<"mechanism">>} == exp_node:fqn( MaybeMech )
			andalso
			exp_text:text_flat( MaybeMech ) == MechNameSought
		end, exp_node_children:get( SaslFeature ) ).




payload_pack_values( [ FirstValue | Values ] ) ->
	[ FirstValue | [ [<<0:8>>, Value] || Value <- Values ] ].


maybe_b64_decode( Bin ) ->
	try base64:decode(Bin)
	catch error:{badarg, _} -> undefined end.
