-module (owl_session_service_muc_client).
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

-export ([
		muc_join/5
	]).

-include_lib("expellee/include/xml.hrl").
-include("log.hrl").
-include("const.hrl").
-include("ns_xmpp_core.hrl").
-include("xmpp_session_events.hrl").

-define( args( Args ), Args ).

-define( get_occupant( OccupantJid ), {get_occupant, OccupantJid} ).


%%% API %%%
get_occupant( Service, OccupantJid ) ->
	gen_server:call( Service, ?get_occupant( OccupantJid ) ).

%%% owl_session_service behaviour %%%

facilities_required( ?args( _ ) ) ->
	{ok, [
			{presence, current_presence, 60000},
			{bind, bound_to_jid, 60000}
		]}.
stream_features_required( ?args( _ ) ) -> {ok, []}.

facilities_provided( ?args( _ ) ) -> {ok, [
		muc_client
	]}.

-record(s, {
		'#module' = ?MODULE :: ?MODULE,
		session_srv :: pid()
	}).

init( ?args( _Args ), SessionSrv ) ->
	{ok, #s{
			session_srv = SessionSrv
		}}.

prerequisites_available( S0 = #s{} ) ->
	ok = owl_session_service:notify_facility_available( muc_client, ok, self() ),
	{ok, S0}.

query_facility( muc_client, S = #s{} ) -> {ok, self(), S}.

handle_call( ?get_occupant( OccupantJid ), ReplyTo, S = #s{} ) ->
	handle_call_get_occupant( OccupantJid, ReplyTo, S );

handle_call( Unexpected, ReplyTo, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_call, {unexpected_request, Unexpected}, {reply_to, ReplyTo} ]),
	{reply, badarg, S}.

handle_cast( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_cast, {unexpected_request, Unexpected} ]),
	{noreply, S}.

% handle_info( ?xmpp_session_event( SessionSrv, SubsID, stanza, ResponseIQ ), S = #s{ session_srv = SessionSrv, subs_id_session_response = SubsID } ) ->
% 	handle_info_stanza_session_response( ResponseIQ, S );

handle_info( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_info, {unexpected_request, Unexpected} ]),
	{noreply, S}.



handle_call_get_occupant( OccupantJid, _ReplyTo, S ) ->
	?log(debug, [?MODULE, handle_call_get_occupant,
			{occupant_jid, OccupantJid}
		]),
	{reply, {error, not_implemented}, S0}.

