-module (owl_xmpp_stream_error).
-compile ({parse_transform, gin}).
-include ("xmpp_stream_error.hrl").
-include ("ns_xmpp_core.hrl").

-export ([
		new/1,
		raise/1,
		property/2,

		stanza_new/1
	]).

new( Props ) ->
	lists:foldl(
			fun new_props_fold/2,
			#xmpp_stream_error{
				common = #xmpp_error_common{},
				public = #xmpp_stream_error_pub{}
			}, Props ).

-spec raise( #xmpp_stream_error{} ) -> no_return().
raise( StreamErr = #xmpp_stream_error{} ) -> error( StreamErr ).

property( Property, Error = #xmpp_stream_error{} ) ->
	case Property of
		text ->
			Error #xmpp_stream_error.public #xmpp_stream_error_pub.text;
		condition ->
			Error #xmpp_stream_error.public #xmpp_stream_error_pub.condition;
		xml_children ->
			Error #xmpp_stream_error.public #xmpp_stream_error_pub.xml_children;
		condition_xml_children ->
			Error #xmpp_stream_error.public #xmpp_stream_error_pub.condition_xml_children;

		'common.stack' ->
			Error #xmpp_stream_error.common #xmpp_error_common.stack;
		'common.origin' ->
			Error #xmpp_stream_error.common #xmpp_error_common.origin;
		'common.reason' ->
			Error #xmpp_stream_error.common #xmpp_error_common.reason
	end.

new_props_fold( Opt, Err ) ->
	case Opt of
 		{text, Text} ->
			?_ensure( is_binary( Text ) orelse Text == undefined ),
			?_alter_field( public, xmpp_stream_error_pub, text, Text );
		{condition, Condition} ->
			?_ensure( ?is_xmpp_stream_error_condition( Condition ) ),
			?_alter_field( public, xmpp_stream_error_pub, condition, Condition );
		{xml_children, Xmls} ->
			?_ensure( is_list( Xmls ) ),
			?_alter_field( public, xmpp_stream_error_pub, xml_children, Xmls );
		{condition_xml_children, Xmls} ->
			?_ensure( is_list( Xmls ) ),
			?_alter_field( public, xmpp_stream_error_pub, condition_xml_children, Xmls );

		{'common.stack', Stack} ->
			?_ensure( is_list(Stack) orelse Stack == undefined ),
			?_alter_field( common, xmpp_error_common, stack, Stack );
		{'common.origin', Origin} ->
			?_alter_field( common, xmpp_error_common, stack, Origin );
		{'common.reason', Reason} ->
			?_alter_field( common, xmpp_error_common, reason, Reason );
		Unknown ->
			Props0 = ( Err #xmpp_stream_error.common ) #xmpp_error_common.unknown_props,
			Props1 = [ Unknown | Props0 ],
			?_alter_field( common, xmpp_error_common, unknown_props, Props1 )
	end.

stanza_new( Err ) ->
	Condition = property( condition, Err ),
	ConditionChildren = property( condition_xml_children, Err ),
	ConditionChild = exp_node:new(
		{?ns_xmpp_streams, atom_to_binary( Condition, latin1 )},
		[], ConditionChildren ),

	Text = property( text, Err ),
	XmlChildren0 = property( xml_children, Err ),
	XmlChildren1 =
		case property( text, Err ) of
			undefined -> XmlChildren0;
			Text ->
				TextChild = exp_node:new(
						{?ns_xmpp_streams, <<"text">>}, [],
						[ exp_text_node:cdata_new( Text ) ]
					),
				[ TextChild | XmlChildren0 ]
		end,
	exp_node:new( {?ns_jabber_streams, <<"error">>}, [], [
			ConditionChild
			| XmlChildren1
		] ).
