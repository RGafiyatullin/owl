-module (owl_stanza_sasl).
-export ([
		stanza/3
	]).

-include_lib ("expellee/include/xml.hrl").
-include ("ns_xmpp_core.hrl").

-spec stanza( xml_ncname(), [ xml_attribute() ], [ binary() ] ) -> xml_element().
stanza( NCN, Attrs, PayloadValues ) ->
	Children =
		case PayloadValues of
			[] -> [];
			[ _ | _ ] ->
				Packed = payload_pack_values( PayloadValues ),
				B64ed = base64:encode( iolist_to_binary(Packed) ),
				[ exp_text_node:cdata_new( B64ed ) ]
		end,
	exp_node:new( {?ns_xmpp_sasl, NCN}, Attrs, Children ).


payload_pack_values( [ FirstValue | Values ] ) ->
	[ FirstValue | [ [<<0:8>>, Value] || Value <- Values ] ].

