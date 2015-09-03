-module (owl_xmpp_session_service_session).
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

-include("log.hrl").
-include("const.hrl").
-include("ns_xmpp_core.hrl").
-include("xmpp_session_events.hrl").

-define(args( Args ), Args ).

facilities_required( ?args( _ ) ) ->
	{ok, [
			{auth, is_authenticated, 30000},
			{bind, bound_to_jid, 60000}
		]}.
stream_features_required( ?args( _ ) ) -> {ok, [
		{?ns_xmpp_session, <<"session">>, []}
	]}.

facilities_provided( ?args( _ ) ) -> {ok, [
		session
	]}.

-record(s, {
		'#module' = ?MODULE :: ?MODULE,
		session_srv :: pid(),

		session_request_timeout = 10000 :: non_neg_integer(),

		session = false :: boolean(),

		subs_id_session_response :: undefined | reference()
	}).

init( ?args( _Args ), SessionSrv ) ->
	{ok, #s{
			session_srv = SessionSrv
		}}.

prerequisites_available( S0 = #s{} ) ->
	IQID = owl_xmpp_stanza:make_id([ iq, ?MODULE, self() ]),
	{ok, S1} = do_subscribe_to_session_response( IQID, S0 ),
	{ok, _S2} = do_send_session_request( IQID, S1 ).

query_facility( session, S = #s{ session = V } ) -> {ok, V, S}.

handle_call( Unexpected, ReplyTo, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_call, {unexpected_request, Unexpected}, {reply_to, ReplyTo} ]),
	{reply, badarg, S}.

handle_cast( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_cast, {unexpected_request, Unexpected} ]),
	{noreply, S}.

handle_info( ?xmpp_session_event( SessionSrv, SubsID, stanza, ResponseIQ ), S = #s{ session_srv = SessionSrv, subs_id_session_response = SubsID } ) ->
	handle_info_stanza_session_response( ResponseIQ, S );

handle_info( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_info, {unexpected_request, Unexpected} ]),
	{noreply, S}.


handle_info_stanza_session_response( ResponseIQ, S0 ) ->
	case owl_xmpp_stanza_iq:type( ResponseIQ ) of
		result ->
			S1 = S0 #s{ session = true },
			ok = owl_xmpp_session_service:notify_facility_available( session, ok, true ),
			{noreply, S1};
		error ->
			ok = owl_xmpp_session_service:notify_facility_available( session, error, ResponseIQ ),
			{stop, {error, ResponseIQ}, S0}
	end.


do_subscribe_to_session_response( IQID, S0 = #s{ session_srv = SessionSrv, session_request_timeout = RequestTimeout } ) ->
	{ok, SubsID} = owl_xmpp_session:subscribe(
		SessionSrv, {fqn_and_id, ?ns_jabber_client, <<"iq">>, IQID},
		self(), ?prio_normal, RequestTimeout ),
	S1 = S0 #s{ subs_id_session_response = SubsID },
	{ok, S1}.


do_send_session_request( IQID, S0 = #s{ session_srv = SessionSrv } ) ->
	SessionElement = exp_node:new( {?ns_xmpp_session, <<"session">>}, [], [] ),
	RequestIQ = owl_xmpp_stanza_iq:request_new( set, IQID, SessionElement ),
	ok = owl_xmpp_session:send_stanza( SessionSrv, RequestIQ ),
	{ok, S0}.
