-module (owl_stanza_error).
-export ([
		new/1,
		raise/1,
		property/2,
		stanza_new/2
	]).
-include("xmpp_stanza_error.hrl").
-include("ns_xmpp_core.hrl").

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
		exp_node:new( {?ns_jabber_streams, <<"error">>}, [], [
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

