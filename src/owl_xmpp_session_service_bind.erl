-module (owl_xmpp_session_service_bind).
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

facilities_required( ?args( _ ) ) -> {ok, [ {auth, is_authenticated, 30000} ]}.
stream_features_required( ?args( _ ) ) -> {ok, [
		{?ns_xmpp_bind, <<"bind">>, []}
	]}.

facilities_provided( ?args( _ ) ) -> {ok, [
		resource_to_bind,
		bound_to_jid
	]}.

-record(s, {
		'#module' = ?MODULE :: ?MODULE,
		session_srv :: pid(),

		bind_request_timeout = 10000 :: non_neg_integer(),

		resource_to_bind :: binary(),
		bound_to_jid :: undefined | owl_xmpp_jid:jid(),

		subs_id_bind_response :: undefined | reference()
	}).

init( ?args( Args ), SessionSrv ) ->
	ResourceToBind = proplists:get_value( resource_to_bind, Args, <<>> ),
	ok = owl_xmpp_session_service:notify_facility_available( resource_to_bind, ok, ResourceToBind ),
	{ok, #s{
			session_srv = SessionSrv,

			resource_to_bind = ResourceToBind
		}}.

prerequisites_available( S0 = #s{} ) ->
	IQID = owl_xmpp_stanza:make_id([ iq, ?MODULE, self() ]),
	{ok, S1} = do_subscribe_to_bind_response( IQID, S0 ),
	{ok, _S2} = do_send_bind_request( IQID, S1 ).

query_facility( resource_to_bind, S = #s{ resource_to_bind = V } ) -> {ok, V, S};
query_facility( bound_to_jid, S = #s{ bound_to_jid = V } ) -> {ok, V, S}.

handle_call( Unexpected, ReplyTo, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_call, {unexpected_request, Unexpected}, {reply_to, ReplyTo} ]),
	{reply, badarg, S}.

handle_cast( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_cast, {unexpected_request, Unexpected} ]),
	{noreply, S}.

handle_info( ?xmpp_session_event( SessionSrv, SubsID, stanza, BindResponseIQ ), S = #s{ session_srv = SessionSrv, subs_id_bind_response = SubsID } ) ->
	handle_info_stanza_bind_response( BindResponseIQ, S );

handle_info( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_info, {unexpected_request, Unexpected} ]),
	{noreply, S}.

handle_info_stanza_bind_response( BindResponseIQ, S0 ) ->
	case owl_xmpp_stanza_iq:type( BindResponseIQ ) of
		result ->
			BoundToJid = bind_response_bound_to_jid( BindResponseIQ ),
			S1 = S0 #s{ bound_to_jid = BoundToJid },
			ok = owl_xmpp_session_service:notify_facility_available( bound_to_jid, ok, BoundToJid ),
			{noreply, S1};
		error ->
			ok = owl_xmpp_session_service:notify_facility_available( bound_to_jid, error, BindResponseIQ ),
			{stop, {error, BindResponseIQ}, S0}
	end.


do_subscribe_to_bind_response( IQID, S0 = #s{ session_srv = SessionSrv, bind_request_timeout = BindRequestTimeout } ) ->
	{ok, SubsID_BindResponse} = owl_xmpp_session:subscribe(
		SessionSrv, {fqn_and_id, ?ns_jabber_client, <<"iq">>, IQID},
		self(), ?prio_normal, BindRequestTimeout ),
	S1 = S0 #s{ subs_id_bind_response = SubsID_BindResponse },
	{ok, S1}.


do_send_bind_request( IQID, S0 = #s{ session_srv = SessionSrv, resource_to_bind = ResourceToBind } ) ->
	BindElement = exp_node:new( {?ns_xmpp_bind, <<"bind">>}, [], [
			exp_node:new( {?ns_xmpp_bind, <<"resource">>}, [], [
					exp_text_node:cdata_new( ResourceToBind )
				] )
		] ),
	BindRequestIQ = owl_xmpp_stanza_iq:request_new( set, IQID, BindElement ),
	ok = owl_xmpp_session:send_stanza( SessionSrv, BindRequestIQ ),
	{ok, S0}.


bind_response_bound_to_jid( BindResponseIQ ) ->
	[ BindElement | _ ] = exp_node_children:get( BindResponseIQ ),
	{ ?ns_xmpp_bind, <<"bind">> } = exp_node:fqn( BindElement ),
	[ JidElement | _ ] = lists:filter(
			fun( MaybeJid ) ->
				exp_node:fqn( MaybeJid ) == {?ns_xmpp_bind, <<"jid">>}
			end,
			exp_node_children:get( BindElement ) ),
	JidBin = exp_text:text_flat( JidElement ),
	_BoundToJid = owl_xmpp_jid:b2j( JidBin ).

