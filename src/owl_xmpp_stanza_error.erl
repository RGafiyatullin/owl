-module (owl_xmpp_stanza_error).
-export ([
		new/1,
		raise/1,
		property/2,
		stanza_new/2
	]).
-export ([
		b2c/1,
		c2b/1
	]).
-include("xmpp_stanza_error.hrl").
-include("ns_xmpp_core.hrl").

-spec b2c( binary() ) -> xmpp_stanza_error_condition().
-spec c2b( xmpp_stanza_error_condition() ) -> binary().

b2c(<<"bad-request">>) -> 'bad-request';
b2c(<<"conflict">>) -> 'conflict';
b2c(<<"feature-not-implemented">>) -> 'feature-not-implemented';
b2c(<<"forbidden">>) -> 'forbidden';
b2c(<<"gone">>) -> 'gone';
b2c(<<"internal-server-error">>) -> 'internal-server-error';
b2c(<<"item-not-found">>) -> 'item-not-found';
b2c(<<"jid-malformed">>) -> 'jid-malformed';
b2c(<<"not-acceptable">>) -> 'not-acceptable';
b2c(<<"not-allowed">>) -> 'not-allowed';
b2c(<<"not-authorized">>) -> 'not-authorized';
b2c(<<"policy-violation">>) -> 'policy-violation';
b2c(<<"recipient-unavailable">>) -> 'recipient-unavailable';
b2c(<<"redirect">>) -> 'redirect';
b2c(<<"registration-required">>) -> 'registration-required';
b2c(<<"remote-server-not-found">>) -> 'remote-server-not-found';
b2c(<<"remote-server-timeout">>) -> 'remote-server-timeout';
b2c(<<"resource-constraint">>) -> 'resource-constraint';
b2c(<<"service-unavailable">>) -> 'service-unavailable';
b2c(<<"subscription-required">>) -> 'subscription-required';
b2c(<<"undefined-condition">>) -> 'undefined-condition';
b2c(<<"unexpected-request">>) -> 'unexpected-request';
b2c(BadCondition) -> error({badarg, stanza_error_condition, BadCondition}).


c2b('bad-request') -> <<"bad-request">>;
c2b('conflict') -> <<"conflict">>;
c2b('feature-not-implemented') -> <<"feature-not-implemented">>;
c2b('forbidden') -> <<"forbidden">>;
c2b('gone') -> <<"gone">>;
c2b('internal-server-error') -> <<"internal-server-error">>;
c2b('item-not-found') -> <<"item-not-found">>;
c2b('jid-malformed') -> <<"jid-malformed">>;
c2b('not-acceptable') -> <<"not-acceptable">>;
c2b('not-allowed') -> <<"not-allowed">>;
c2b('not-authorized') -> <<"not-authorized">>;
c2b('policy-violation') -> <<"policy-violation">>;
c2b('recipient-unavailable') -> <<"recipient-unavailable">>;
c2b('redirect') -> <<"redirect">>;
c2b('registration-required') -> <<"registration-required">>;
c2b('remote-server-not-found') -> <<"remote-server-not-found">>;
c2b('remote-server-timeout') -> <<"remote-server-timeout">>;
c2b('resource-constraint') -> <<"resource-constraint">>;
c2b('service-unavailable') -> <<"service-unavailable">>;
c2b('subscription-required') -> <<"subscription-required">>;
c2b('undefined-condition') -> <<"undefined-condition">>;
c2b('unexpected-request') -> <<"unexpected-request">>.


new( Props ) ->
	lists:foldl(
			fun new_props_fold/2,
			#xmpp_stanza_error{
				common = #xmpp_error_common{},
				public = #xmpp_stanza_error_pub{}
			}, Props ).


-spec raise( #xmpp_stanza_error{} ) -> no_return().
raise( Err = #xmpp_stanza_error{} ) -> error( Err ).

property( Property, Error = #xmpp_stanza_error{} ) ->
	case Property of
		text ->
			Error #xmpp_stanza_error.public #xmpp_stanza_error_pub.text;
		condition ->
			Error #xmpp_stanza_error.public #xmpp_stanza_error_pub.condition;
		code ->
			Error #xmpp_stanza_error.public #xmpp_stanza_error_pub.code;
		type ->
			Error #xmpp_stanza_error.public #xmpp_stanza_error_pub.type;
		xml_children ->
			Error #xmpp_stanza_error.public #xmpp_stanza_error_pub.xml_children;
		condition_xml_children ->
			Error #xmpp_stanza_error.public #xmpp_stanza_error_pub.condition_xml_children;
		stanza_xml_children ->
			Error #xmpp_stanza_error.public #xmpp_stanza_error_pub.stanza_xml_children;

		'common.stack' ->
			Error #xmpp_stanza_error.common #xmpp_error_common.stack;
		'common.origin' ->
			Error #xmpp_stanza_error.common #xmpp_error_common.origin;
		'common.reason' ->
			Error #xmpp_stanza_error.common #xmpp_error_common.reason
	end.

stanza_new( Err, RequestStanza ) ->
	Condition = property( condition, Err ),
	ConditionChildren = property( condition_xml_children, Err ),
	ConditionChild = exp_node:new(
		{?ns_xmpp_stanzas, atom_to_binary( Condition, latin1 )},
		[], ConditionChildren ),

	Text = property( text, Err ),
	XmlChildren0 = property( xml_children, Err ),
	XmlChildren1 =
		case property( text, Err ) of
			undefined -> XmlChildren0;
			Text ->
				TextChild = exp_node:new(
						{?ns_xmpp_stanzas, <<"text">>}, [],
						[ exp_text_node:cdata_new( Text ) ]
					),
				[ TextChild | XmlChildren0 ]
		end,

	ErrorSubElement =
		exp_node:new( {?ns_jabber_client, <<"error">>}, [], [
			ConditionChild
			| XmlChildren1
		] ),

	MaybeIDAttr = maybe_attr( <<"id">>, <<"id">>, RequestStanza ),
	MaybeToAttr = maybe_attr( <<"to">>, <<"from">>, RequestStanza ),
	MaybeFromAttr = maybe_attr( <<"from">>, <<"to">>, RequestStanza ),

	Attrs = MaybeIDAttr ++ MaybeToAttr ++ MaybeFromAttr,

	StanzaFQN = exp_node:fqn( RequestStanza ),
	_ResponseStanza =
		exp_node:new( StanzaFQN,
			[ {<<"type">>, <<"error">>} ] ++ Attrs,
			[ ErrorSubElement ] ).



maybe_attr( NewAttrName, OldAttrName, RequestStanza ) ->
	case exp_node_attrs:attr( OldAttrName, RequestStanza ) of
		undefined -> [];
		Defined -> [ {NewAttrName, Defined} ]
	end.




-define( _ensure( Expr ), case (Expr) of true -> ok; _ -> error( {badarg, ??Expr} ) end ).
-define( _alter_field( FirstField, AsRecord, SecondField, Value ),
			(Err #xmpp_stanza_error{
				FirstField = (
					( Err #xmpp_stanza_error.FirstField )
						#AsRecord{ SecondField = Value }
					)
				}
			)
		).

new_props_fold( Opt, Err ) ->
	case Opt of
		{text, Text} ->
			?_ensure( is_binary( Text ) orelse Text == undefined ),
			?_alter_field( public, xmpp_stanza_error_pub, text, Text );
		{condition, Condition} ->
			?_ensure( ?is_xmpp_stanza_error_condition( Condition ) ),
			?_alter_field( public, xmpp_stanza_error_pub, condition, Condition );
		{code, Code} ->
			?_ensure( is_integer( Code ) orelse Code == undefined ),
			?_alter_field( public, xmpp_stanza_error_pub, code, Code );
		{type, Type} ->
			?_ensure( ?is_xmpp_stanza_error_type( Type ) ),
			?_alter_field( public, xmpp_stanza_error_pub, type, Type );
		{xml_children, Xmls} ->
			?_ensure( is_list( Xmls ) ),
			?_alter_field( public, xmpp_stanza_error_pub, xml_children, Xmls );
		{condition_xml_children, Xmls} ->
			?_ensure( is_list( Xmls ) ),
			?_alter_field( public, xmpp_stanza_error_pub, condition_xml_children, Xmls );
		{stanza_xml_children, Xmls} ->
			?_ensure( is_list( Xmls ) ),
			?_alter_field( public, xmpp_stanza_error_pub, stanza_xml_children, Xmls );

		{'common.stack', Stack} ->
			?_ensure( is_list(Stack) orelse Stack == undefined ),
			?_alter_field( common, xmpp_error_common, stack, Stack );
		{'common.origin', Origin} ->
			?_alter_field( common, xmpp_error_common, stack, Origin );
		{'common.reason', Reason} ->
			?_alter_field( common, xmpp_error_common, reason, Reason );

		Unknown ->
			Props0 = ( Err #xmpp_stanza_error.common ) #xmpp_error_common.unknown_props,
			Props1 = [ Unknown | Props0 ],
			?_alter_field( common, xmpp_error_common, unknown_props, Props1 )
	end.

