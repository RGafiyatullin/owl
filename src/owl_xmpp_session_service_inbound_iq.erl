-module (owl_xmpp_session_service_inbound_iq).
-compile({parse_transform, gin}).
-behaviour (owl_xmpp_session_service).

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

-include_lib("expellee/include/xml.hrl").
-include("log.hrl").
-include("const.hrl").
-include("ns_xmpp_core.hrl").
-include("xmpp_stanza_error.hrl").
-include("xmpp_session_events.hrl").

-define(args( Args ), Args ).

facilities_required( ?args( _ ) ) -> {ok, []}.
stream_features_required( ?args( _ ) ) -> {ok, []}.
facilities_provided( ?args( _ ) ) -> {ok, []}.

-record(s, {
		'#module' = ?MODULE :: ?MODULE,
		session_srv :: owl_xmpp:session_srv(),

		subs_id_iq :: undefined | reference()
	}).

init( ?args( _Args ), SessionSrv ) ->
	{ok, #s{
			session_srv = SessionSrv
		}}.

prerequisites_available( S0 = #s{} ) ->
	{ok, _S1} = do_subscribe_to_inbound_iq( S0 ).

query_facility( _, S = #s{} ) -> {ok, undefined, S}.

handle_call( Unexpected, ReplyTo, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_call, {unexpected_request, Unexpected}, {reply_to, ReplyTo} ]),
	{reply, badarg, S}.

handle_cast( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_cast, {unexpected_request, Unexpected} ]),
	{noreply, S}.

handle_info( ?xmpp_session_event( SessionSrv, SubsID, stanza, InboundIQ ), S = #s{ session_srv = SessionSrv, subs_id_iq = SubsID } ) ->
	handle_info_stanza_inbound_iq( InboundIQ, S );

handle_info( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_info, {unexpected_request, Unexpected} ]),
	{noreply, S}.

handle_info_stanza_inbound_iq( RequestIQ, S0 = #s{ session_srv = SessionSrv } ) ->
	?log(debug, [ ?MODULE, handle_info_stanza_inbound_iq, {request_iq, RequestIQ} ]),
	ResponseIQ = owl_xmpp_stanza_error:stanza_new( ?stanza_error([
			{condition, 'service-unavailable'},
			{text, <<"Unhandled IQ">>}
		]), RequestIQ ),
	ok = owl_xmpp_session:send_stanza( SessionSrv, ResponseIQ ),
	{noreply, S0}.

do_subscribe_to_inbound_iq( S0 = #s{ subs_id_iq = SubsID } ) when is_reference( SubsID ) ->
	{ok, S0};
do_subscribe_to_inbound_iq( S0 = #s{ session_srv = SessionSrv, subs_id_iq = undefined } ) ->
	{ok, SubsID} = owl_xmpp_session:subscribe(
		SessionSrv, {fqn, ?ns_jabber_client, <<"iq">>},
		[ fun( IQ ) -> in( owl_xmpp_stanza_iq:type( IQ ), [ get, set ] ) end ],
		self(), ?prio_normal, infinity ),
	S1 = S0 #s{ subs_id_iq = SubsID },
	{ok, S1}.


