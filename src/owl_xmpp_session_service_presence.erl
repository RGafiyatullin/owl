-module (owl_xmpp_session_service_presence).
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
-include("xmpp_session_events.hrl").

-define(args( Args ), Args ).

facilities_required( ?args( _ ) ) ->
	{ok, [
			{auth, is_authenticated, 30000},
			{bind, bound_to_jid, 60000},
			{session, session, 60000}
		]}.
stream_features_required( ?args( _ ) ) -> {ok, []}.

facilities_provided( ?args( _ ) ) -> {ok, [ current_presence ]}.

-record(s, {
		'#module' = ?MODULE :: ?MODULE,
		session_srv :: owl_xmpp:session_srv(),

		current_presence = undefined :: undefined | xml_element(),

		subs_id_presence :: undefined | reference(),

		bound_to_jid :: undefined | owl_xmpp_jid:jid(),
		seq_id = 0 :: non_neg_integer()
	}).

init( ?args( _Args ), SessionSrv ) ->
	{ok, #s{
			session_srv = SessionSrv
		}}.

prerequisites_available( S0 = #s{} ) ->
	{ok, S1} = do_subscribe_to_presence_stanzas( S0 ),
	{ok, S2} = do_send_presence( S1 ),
	{ok, _S3} = do_query_jid_bound_to( S2 ).

query_facility( current_presence, S = #s{ current_presence = V } ) -> {ok, V, S}.

handle_call( Unexpected, ReplyTo, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_call, {unexpected_request, Unexpected}, {reply_to, ReplyTo} ]),
	{reply, badarg, S}.

handle_cast( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_cast, {unexpected_request, Unexpected} ]),
	{noreply, S}.

handle_info( ?xmpp_session_event( SessionSrv, SubsID, stanza, Presence ), S = #s{ session_srv = SessionSrv, subs_id_presence = SubsID } ) ->
	handle_info_stanza_presence( Presence, S );

handle_info( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_info, {unexpected_request, Unexpected} ]),
	{noreply, S}.


handle_info_stanza_presence( Presence, S0 = #s{ bound_to_jid = BoundToJid } ) ->
	PresenceFrom = owl_xmpp_stanza:from( Presence ),
	?log(debug, [ ?MODULE, handle_info_stanza_presence, {presence_from, PresenceFrom}, {bound_to_jid, BoundToJid}, {presence, Presence} ]),
	S1 =
		case {PresenceFrom == BoundToJid, owl_xmpp_stanza_presence:type( Presence )} of
			{false, _} -> S0;

			{true, error} ->
				ok = owl_xmpp_session_service:notify_facility_available( current_presence, error, Presence ),
				S0;

			{true, available} ->
				ok = owl_xmpp_session_service:notify_facility_available( current_presence, ok, Presence ),
				S0 #s{ current_presence = Presence };

			{_, _} -> S0
		end,
	{noreply, S1}.

do_subscribe_to_presence_stanzas( S0 = #s{ session_srv = SessionSrv } ) ->
	{ok, SubsID} = owl_xmpp_session:subscribe(
		SessionSrv, {fqn, ?ns_jabber_client, <<"presence">>},
		self(), ?prio_normal, infinity ),
	S1 = S0 #s{ subs_id_presence = SubsID },
	{ok, S1}.


do_send_presence( S0 = #s{ session_srv = SessionSrv } ) ->
	{SeqID, S1} = next_seq_id( S0 ),
	PresenceID = owl_xmpp_stanza:make_id( [ presence, ?MODULE, self(), SeqID ] ),
	Presence = owl_xmpp_stanza_presence:stanza_new( available, [ {<<"id">>, PresenceID} ] ),
	ok = owl_xmpp_session:send_stanza( SessionSrv, Presence ),
	{ok, S1}.

do_query_jid_bound_to( S0 = #s{ session_srv = SessionSrv } ) ->
	{ok, BoundToJid} = owl_xmpp_session_service:query_facility( SessionSrv, bind, bound_to_jid, ?call_timeout_short ),
	S1 = S0 #s{ bound_to_jid = BoundToJid },
	{ok, S1}.


next_seq_id( S = #s{ seq_id = SeqID } ) ->
	{SeqID, S #s{ seq_id = SeqID + 1 }}.

