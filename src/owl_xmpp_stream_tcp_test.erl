-module (owl_xmpp_stream_tcp_test).
-include_lib("eunit/include/eunit.hrl").

-include("xmpp_stream_events.hrl").
-include("ns_xmpp_core.hrl").

t1_stanza_sasl_test() ->
	NCN = <<"auth">>,
	Attrs = [{<<"mechanisms">>, <<"PLAIN">>}],
	Values = [ <<"a@localhost">>, <<"a">>, <<"pw">> ],

	?assert(
		{ok, NCN, Attrs, Values}
		== owl_xmpp_stanza_sasl:parse_stanza(
			owl_xmpp_stanza_sasl:stanza_new( NCN, Attrs, Values ) ) ).

t2_stanza_presence_test() ->
	PresenceAvailable = owl_xmpp_stanza_presence:stanza_new( available ),
	?assert( true == owl_xmpp_stanza_presence:is_presence( PresenceAvailable ) ),
	?assert( available == owl_xmpp_stanza_presence:type( PresenceAvailable ) ).


owl_xmpp_stream_tcp_active_true_test() ->
	{ok, Owl} = owl_xmpp_stream_tcp:start_link( tcp_connect, "localhost", 5222, [] ),
	ok = owl_xmpp_stream_tcp:set_active( Owl, true ),
	ok = owl_xmpp_stream_tcp:send_stream_open( Owl, [ {<<"to">>, <<"admin.localhost">>} ], true ),

	{ok, ?xmpp_stream_open( Owl, _StreamAttrs )} = owl_xmpp_stream_tcp:receive_xmpp_event( Owl ),

	{ok, ?xmpp_stanza(Owl, MaybeFeatures)} = owl_xmpp_stream_tcp:receive_xmpp_event( Owl ),
	?assert( true == owl_xmpp_stanza_stream_features:is_stanza_stream_features( MaybeFeatures ) ),
	{ok, SaslFeature} = owl_xmpp_stanza_stream_features:get_feature( ?ns_xmpp_sasl, <<"mechanisms">>, MaybeFeatures ),
	?assert( true == owl_xmpp_stanza_sasl:feature_mechanisms_has_mech( <<"PLAIN">>, SaslFeature ) ),

	SaslAuthRequest1 = owl_xmpp_stanza_sasl:stanza_new( <<"auth">>, [ {<<"mechanism">>, <<"PLAIN">>} ], [ <<>>, <<"a">>, <<"pw">> ] ),
	ok = owl_xmpp_stream_tcp:send_stanza( Owl, SaslAuthRequest1 ),
	{ok, ?xmpp_stanza( Owl, MaybeSaslAuthResponse1 )} = owl_xmpp_stream_tcp:receive_xmpp_event( Owl ),
	?assert( {?ns_xmpp_sasl, <<"failure">>} == exp_node:fqn( MaybeSaslAuthResponse1 ) ),

	SaslAuthRequest2 = owl_xmpp_stanza_sasl:stanza_new( <<"auth">>, [ {<<"mechanism">>, <<"PLAIN">>} ], [ <<"admin@admin.localhost">>, <<"admin">>, <<"admin-pw">> ] ),
	ok = owl_xmpp_stream_tcp:send_stanza( Owl, SaslAuthRequest2 ),
	{ok, ?xmpp_stanza( Owl, MaybeSaslAuthResponse2 )} = owl_xmpp_stream_tcp:receive_xmpp_event( Owl ),
	?assert( {?ns_xmpp_sasl, <<"success">>} == exp_node:fqn( MaybeSaslAuthResponse2 ) ),

	ok = timer:sleep(100),
	ok = owl_xmpp_stream_tcp:send_stream_close( Owl ),
	{ok, Owl}.

owl_xmpp_stream_tcp_active_once_test() ->
	{ok, Owl} = owl_xmpp_stream_tcp:start_link( tcp_connect, "localhost", 5222, [] ),
	ok = owl_xmpp_stream_tcp:send_stream_open( Owl, [ {<<"to">>, <<"admin.localhost">>} ], true ),

	ok = owl_xmpp_stream_tcp:set_active( Owl, once ),
	{ok, ?xmpp_stream_open( Owl, _StreamAttrs1 )} = owl_xmpp_stream_tcp:receive_xmpp_event( Owl ),

	ok = owl_xmpp_stream_tcp:set_active( Owl, once ),
	{ok, ?xmpp_stanza(Owl, MaybeFeatures)} = owl_xmpp_stream_tcp:receive_xmpp_event( Owl ),

	?assert( true == owl_xmpp_stanza_stream_features:is_stanza_stream_features( MaybeFeatures ) ),
	{ok, SaslFeature} = owl_xmpp_stanza_stream_features:get_feature( ?ns_xmpp_sasl, <<"mechanisms">>, MaybeFeatures ),
	?assert( true == owl_xmpp_stanza_sasl:feature_mechanisms_has_mech( <<"PLAIN">>, SaslFeature ) ),

	SaslAuthRequest1 = owl_xmpp_stanza_sasl:stanza_new( <<"auth">>, [ {<<"mechanism">>, <<"PLAIN">>} ], [ <<>>, <<"a">>, <<"pw">> ] ),
	ok = owl_xmpp_stream_tcp:send_stanza( Owl, SaslAuthRequest1 ),
	ok = owl_xmpp_stream_tcp:set_active( Owl, once ),
	{ok, ?xmpp_stanza( Owl, MaybeSaslAuthResponse1 )} = owl_xmpp_stream_tcp:receive_xmpp_event( Owl ),
	?assert( {?ns_xmpp_sasl, <<"failure">>} == exp_node:fqn( MaybeSaslAuthResponse1 ) ),

	SaslAuthRequest2 = owl_xmpp_stanza_sasl:stanza_new( <<"auth">>, [ {<<"mechanism">>, <<"PLAIN">>} ], [ <<"admin@admin.localhost">>, <<"admin">>, <<"admin-pw">> ] ),
	ok = owl_xmpp_stream_tcp:send_stanza( Owl, SaslAuthRequest2 ),
	ok = owl_xmpp_stream_tcp:set_active( Owl, once ),
	{ok, ?xmpp_stanza( Owl, MaybeSaslAuthResponse2 )} = owl_xmpp_stream_tcp:receive_xmpp_event( Owl ),
	?assert( {?ns_xmpp_sasl, <<"success">>} == exp_node:fqn( MaybeSaslAuthResponse2 ) ),

	ok = owl_xmpp_stream_tcp:send_stream_open( Owl, [ {<<"to">>, <<"admin.localhost">>} ], true ),
	ok = owl_xmpp_stream_tcp:set_active( Owl, once ),
	{ok, ?xmpp_stream_open( Owl, _StreamAttrs2 )} = owl_xmpp_stream_tcp:receive_xmpp_event( Owl ),

	ok = owl_xmpp_stream_tcp:set_active( Owl, true ),
	PresenceRequest = owl_xmpp_stanza_presence:stanza_new( available ),
	ok = owl_xmpp_stream_tcp:send_stanza( Owl, PresenceRequest ),
	{ok, ?xmpp_stanza( Owl, _PresenceEcho )} = owl_xmpp_stream_tcp:receive_xmpp_event( Owl ),



	ok = timer:sleep(100),
	ok = owl_xmpp_stream_tcp:send_stream_close( Owl ),
	{ok, Owl}.


