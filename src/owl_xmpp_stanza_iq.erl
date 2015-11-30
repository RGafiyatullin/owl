-module (owl_xmpp_stanza_iq).
-compile ({parse_transform, gin}).
-export ([
		request_new/3,

		type/1, is_iq/1,
		query_fqn/1,

		type2bin/1, bin2type/1
	]).
-include_lib("expellee/include/xml.hrl").
-include("ns_xmpp_core.hrl").

-define( all_iq_types, [ get, set, result, error ] ).
-define( all_iq_types_bin, [ <<"get">>, <<"set">>, <<"result">>, <<"error">> ] ).

-spec request_new( get|set, binary(), #xe{} ) -> #xe{}.
request_new( Type, ID, QueryElement )
	when in(Type, [get, set])
	andalso is_binary( ID )
	andalso ?is_xml_xe( QueryElement )
->
	IQAttrs = [ {<<"type">>, type2bin( Type )}, {<<"id">>, ID} ],
	exp_node:new( {?ns_jabber_client, <<"iq">>}, IQAttrs, [ QueryElement ] ).


type( IQ ) ->
	true = is_iq( IQ ),
	TypeBin = exp_node_attrs:attr( <<"type">>, IQ ),
	bin2type( TypeBin ).

is_iq( MaybeIQ ) ->
	{?ns_jabber_client, <<"iq">>} == exp_node:fqn( MaybeIQ ).

query_fqn( IQ ) ->
	case exp_node_children:get( IQ ) of
		[] -> {undefined, undefined};
		[ QueryXml | _ ] ->
			exp_node:fqn( QueryXml )
	end.


type2bin( T ) when in( T, ?all_iq_types ) -> atom_to_binary( T, latin1 );
type2bin( BadArg ) -> error({badarg, iq_type, BadArg}).

bin2type( B ) when in( B, ?all_iq_types_bin ) -> binary_to_existing_atom( B, latin1 );
bin2type( BadArg ) -> error({badarg, iq_type_bin, BadArg}).



