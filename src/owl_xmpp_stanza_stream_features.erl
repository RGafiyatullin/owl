-module (owl_xmpp_stanza_stream_features).
-export ([
		is_stanza_stream_features/1,
		get_feature/3
	]).

-include_lib ("expellee/include/xml.hrl").
-include ("ns_xmpp_core.hrl").

-spec is_stanza_stream_features( xml_element() ) -> boolean().
is_stanza_stream_features( Xml ) ->
	{?ns_jabber_streams, <<"features">>} == exp_node:fqn( Xml ).


-spec get_feature( xml_ns(), xml_ncname(), xml_element() ) -> error | {ok, xml_element()}.
get_feature( NS, NCN, StreamFeatures ) ->
	true = is_stanza_stream_features( StreamFeatures ),
	Features = exp_node_children:get( StreamFeatures ),
	case lists:filter( fun( Feature ) ->
			exp_node:fqn( Feature ) == {NS, NCN}
		end, Features )
	of
		[] -> error;
		[ Feature | _ ] -> {ok, Feature}
	end.



