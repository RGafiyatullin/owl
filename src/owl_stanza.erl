-module (owl_stanza).
-export ([
		id/1, set_id/2
	]).

id( Xml ) ->
	exp_node_attrs:attr( <<"id">>, Xml ).

set_id( ID, Xml ) when ID == undefined orelse is_binary( ID ) ->
	exp_node_attrs:set_attr( <<"id">>, ID, Xml ).

