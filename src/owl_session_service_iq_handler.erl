-module (owl_session_service_iq_handler).
-compile ({parse_transform, gin}).
-behaviour (owl_session_service).

-export ([
		facilities_required/1,
		stream_features_required/1,

		prerequisites_available/1,

		query_facility/2,
		facilities_provided/1,

		init/2,
		handle_call/3,
		handle_cast/2,
		handle_info/2
	]).

-include("log.hrl").
-include("const.hrl").
-include("ns_xmpp_core.hrl").
-include("xmpp_session_events.hrl").
-include("xmpp_stanza_error.hrl").

-define(args( Args ), Args ).

facilities_required( ?args( _ ) ) ->
	{ok, []}.
stream_features_required( ?args( _ ) ) -> {ok, []}.

facilities_provided( ?args( _ ) ) -> {ok, [
		iq_handler
	]}.

-record(s, {
		'#module' = ?MODULE :: ?MODULE,
		session_srv :: pid(),
		subs_id_inbound_iq :: undefined | reference()
	}).

init( ?args( _Args ), SessionSrv ) ->
	{ok, #s{
			session_srv = SessionSrv
		}}.

prerequisites_available( S0 = #s{} ) ->
	ok = owl_session_service:notify_facility_available( iq_handler, ok, self() ),
	{ok, _S1} = do_subscribe_to_inbound_iqs( S0 ).

query_facility( muc_client, S = #s{} ) -> {ok, self(), S}.

handle_call( Unexpected, ReplyTo, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_call, {unexpected_request, Unexpected}, {reply_to, ReplyTo} ]),
	{reply, badarg, S}.

handle_cast( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_cast, {unexpected_request, Unexpected} ]),
	{noreply, S}.

handle_info( ?xmpp_session_event( SessionSrv, SubsID, stanza, InboundIQ ), S = #s{ session_srv = SessionSrv, subs_id_inbound_iq = SubsID } ) ->
	handle_info_stanza_inbound_iq( InboundIQ, S );

handle_info( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_info, {unexpected_request, Unexpected} ]),
	{noreply, S}.


do_subscribe_to_inbound_iqs( S0 = #s{ session_srv = SessionSrv } ) ->
	{ok, SubsID} = owl_session:subscribe(
		SessionSrv, {fqn, ?ns_jabber_client, <<"iq">>}, [
			fun( IQ ) -> in(owl_stanza_iq:type( IQ ), [ get, set ]) end
		],
		self(), ?prio_normal, infinity ),
	S1 = S0 #s{ subs_id_inbound_iq = SubsID },
	{ok, S1}.

handle_info_stanza_inbound_iq( InboundIQ, S0 = #s{} ) ->
	{ok, S1} = do_send_response_service_unavailable( InboundIQ, S0 ),
	{noreply, S1}.

do_send_response_service_unavailable( InboundIQ, S0 = #s{ session_srv = SessionSrv } ) ->
	ServiceUnavailableError = ?stanza_error([
			{condition, 'service-unavailable'},
			{text, <<"Unhandled IQ">>}
		]),
	ResponseIQ = owl_stanza_error:stanza_new( ServiceUnavailableError, InboundIQ ),
	owl_session:send_stanza( SessionSrv, ResponseIQ ),
	{ok, S0}.

