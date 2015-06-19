-module (owl_test).
-export ([prepare/0]).
-include_lib("eunit/include/eunit.hrl").

-include("xmpp_stream_events.hrl").
-include("ns_xmpp_core.hrl").

prepare() ->
	% application:start(sasl).
	ok.

t1_stanza_sasl_test() ->
	NCN = <<"auth">>,
	Attrs = [{<<"mechanisms">>, <<"PLAIN">>}],
	Values = [ <<"a@localhost">>, <<"a">>, <<"pw">> ],

	?assert(
		{ok, NCN, Attrs, Values}
		== owl_stanza_sasl:parse_stanza(
			owl_stanza_sasl:stanza_new( NCN, Attrs, Values ) ) ).

t2_stanza_presence_test() ->
	PresenceAvailable = owl_stanza_presence:stanza_new( available ),
	?assert( true == owl_stanza_presence:is_presence( PresenceAvailable ) ),
	?assert( available == owl_stanza_presence:type( PresenceAvailable ) ).


t100_test() ->
	{ok, Owl} = owl_tcp:start_link( tcp_connect, "localhost", 5222, [] ),
	ok = owl_tcp:set_active( Owl, true ),
	ok = owl_tcp:send_stream_open( Owl, [ {<<"to">>, <<"admin.localhost">>} ] ),

	{ok, ?xmpp_stream_open( Owl, _StreamAttrs )} = owl_tcp:receive_xmpp_event( Owl ),

	{ok, ?xmpp_stanza(Owl, MaybeFeatures)} = owl_tcp:receive_xmpp_event( Owl ),
	?assert( true == owl_stanza_stream_features:is_stanza_stream_features( MaybeFeatures ) ),
	{ok, SaslFeature} = owl_stanza_stream_features:get_feature( ?ns_xmpp_sasl, <<"mechanisms">>, MaybeFeatures ),
	?assert( true == owl_stanza_sasl:feature_mechanisms_has_mech( <<"PLAIN">>, SaslFeature ) ),

	SaslAuthRequest = owl_stanza_sasl:stanza_new( <<"auth">>, [ {<<"mechanism">>, <<"PLAIN">>} ], [ <<>>, <<"a">>, <<"pw">> ] ),
	ok = owl_tcp:send_stanza( Owl, SaslAuthRequest ),
	{ok, ?xmpp_stanza( Owl, MaybeSaslAuthResponse )} = owl_tcp:receive_xmpp_event( Owl ),
	?assert( {?ns_xmpp_sasl, <<"failure">>} == exp_node:fqn( MaybeSaslAuthResponse ) ),

	ok = timer:sleep(1000),
	ok = owl_tcp:send_stream_close( Owl ),
	{ok, Owl}.



