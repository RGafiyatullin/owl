-module (owl_xmpp_stream_error).
-compile ({parse_transform, gin}).
-export_type([
		stream_error/0
	]).
-export ([
		new/1,
		raise/1,
		property/2,

		stanza_new/1,
		stanza_parse/1
	]).
-export ([
		b2c/1,
		c2b/1
	]).
-include ("xmpp_stream_error.hrl").
-include ("ns_xmpp_core.hrl").

-type stream_error() :: #xmpp_stream_error{}.

-spec b2c( binary() ) -> xmpp_stream_error_condition().
-spec c2b( xmpp_stream_error_condition() ) -> binary().

b2c(<<"bad-format">>) -> 'bad-format';
b2c(<<"bad-namespace-prefix">>) -> 'bad-namespace-prefix';
b2c(<<"conflict">>) -> 'conflict';
b2c(<<"connection-timeout">>) -> 'connection-timeout';
b2c(<<"host-gone">>) -> 'host-gone';
b2c(<<"host-unknown">>) -> 'host-unknown';
b2c(<<"improper-addressing">>) -> 'improper-addressing';
b2c(<<"internal-server-error">>) -> 'internal-server-error';
b2c(<<"invalid-form">>) -> 'invalid-form';
b2c(<<"invalid-id">>) -> 'invalid-id';
b2c(<<"invalid-namespace">>) -> 'invalid-namespace';
b2c(<<"invalid-xml">>) -> 'invalid-xml';
b2c(<<"not-authorized">>) -> 'not-authorized';
b2c(<<"policy-violation">>) -> 'policy-violation';
b2c(<<"remote-connection-failed">>) -> 'remote-connection-failed';
b2c(<<"resource-constraint">>) -> 'resource-constraint';
b2c(<<"restricted-xml">>) -> 'restricted-xml';
b2c(<<"see-other-host">>) -> 'see-other-host';
b2c(<<"system-shutdown">>) -> 'system-shutdown';
b2c(<<"undefined-condition">>) -> 'undefined-condition';
b2c(<<"unsupported-encoding">>) -> 'unsupported-encoding';
b2c(<<"unsupported-stanza-type">>) -> 'unsupported-stanza-type';
b2c(<<"unsupported-version">>) -> 'unsupported-version';
b2c(<<"not-well-formed">>) -> 'not-well-formed';
b2c(BadCondition) -> error({badarg, stream_error_condition, BadCondition}).


c2b('bad-format') -> <<"bad-format">>;
c2b('bad-namespace-prefix') -> <<"bad-namespace-prefix">>;
c2b('conflict') -> <<"conflict">>;
c2b('connection-timeout') -> <<"connection-timeout">>;
c2b('host-gone') -> <<"host-gone">>;
c2b('host-unknown') -> <<"host-unknown">>;
c2b('improper-addressing') -> <<"improper-addressing">>;
c2b('internal-server-error') -> <<"internal-server-error">>;
c2b('invalid-form') -> <<"invalid-form">>;
c2b('invalid-id') -> <<"invalid-id">>;
c2b('invalid-namespace') -> <<"invalid-namespace">>;
c2b('invalid-xml') -> <<"invalid-xml">>;
c2b('not-authorized') -> <<"not-authorized">>;
c2b('policy-violation') -> <<"policy-violation">>;
c2b('remote-connection-failed') -> <<"remote-connection-failed">>;
c2b('resource-constraint') -> <<"resource-constraint">>;
c2b('restricted-xml') -> <<"restricted-xml">>;
c2b('see-other-host') -> <<"see-other-host">>;
c2b('system-shutdown') -> <<"system-shutdown">>;
c2b('undefined-condition') -> <<"undefined-condition">>;
c2b('unsupported-encoding') -> <<"unsupported-encoding">>;
c2b('unsupported-stanza-type') -> <<"unsupported-stanza-type">>;
c2b('unsupported-version') -> <<"unsupported-version">>;
c2b('not-well-formed') -> <<"not-well-formed">>.

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

stanza_parse( Stanza ) ->
	{ok, {stub_for_parsing_stream_error, Stanza}}.

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

-define( _ensure( Expr ), case (Expr) of true -> ok; _ -> error( {badarg, ??Expr} ) end ).
-define( _alter_field( FirstField, AsRecord, SecondField, Value ),
			(Err #xmpp_stream_error{
				FirstField = (
					( Err #xmpp_stream_error.FirstField )
						#AsRecord{ SecondField = Value }
					)
				}
			)
		).

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
