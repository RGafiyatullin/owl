-module (owl_xmpp_stanza_iq).
-compile ({parse_transform, gin}).
-export ([
		request_new/3,
		response_new/2,

		type/1, is_iq/1,
		query_fqn/1,

		type2bin/1, bin2type/1
	]).
-include_lib("expellee/include/xml.hrl").
-include("ns_xmpp_core.hrl").

-define( all_iq_types, [ get, set, result, error ] ).
-define( all_iq_types_bin, [ <<"get">>, <<"set">>, <<"result">>, <<"error">> ] ).

-spec request_new( get|set, binary(), owl_xmpp:xml_element() ) -> owl_xmpp:xml_element().
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

-spec response_new(
		RequestIQ :: owl_xmpp:xml_element(),
		MaybeResponseBody :: undefined | owl_xmpp:xml_element()
	) ->
		ResponseIQ :: owl_xmpp:xml_element().

response_new( ReqIQ, RespBody ) ->
	true = is_iq(ReqIQ),
	ReqType = type(ReqIQ),
	true = ( ReqType == get orelse ReqType == set ),
	ID = owl_xmpp_stanza:id(ReqIQ),
	exp_node:new(
		{?ns_jabber_client, <<"iq">>},
		[{<<"id">>, ID}, {<<"type">>, <<"result">>}],
		case RespBody of
			undefined -> [];
			_ -> [RespBody]
		end).


type2bin( T ) when in( T, ?all_iq_types ) -> atom_to_binary( T, latin1 );
type2bin( BadArg ) -> error({badarg, iq_type, BadArg}).

bin2type( B ) when in( B, ?all_iq_types_bin ) -> binary_to_existing_atom( B, latin1 );
bin2type( BadArg ) -> error({badarg, iq_type_bin, BadArg}).



