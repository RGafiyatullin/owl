-module (owl_session_service_sasl_auth).
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

-define(match_sasl_auth_response, {ns, ?ns_xmpp_sasl}).
-define(args( AuthData ), AuthData ).

facilities_required( ?args( _ ) ) -> {ok, []}.
stream_features_required( ?args( _ ) ) -> {ok, [
		{?ns_xmpp_sasl, <<"mechanisms">>, [ fun_any_of_mechs_available( [ <<"PLAIN">> ] ) ]}
	]}.
facilities_provided( ?args( _ ) ) -> {ok, [
		auth_zid,
		auth_cid,
		auth_host,
		auth_secret,
		is_authenticated
	]}.

-record(s, {
		'#module' = ?MODULE :: ?MODULE,

		session_srv :: pid(),

		auth_zid = <<>> :: binary(),
		auth_cid = <<>> :: binary(),
		auth_host = <<>> :: binary(),
		auth_secret = <<>> :: binary(),

		is_authenticated = false :: boolean(),

		subs_id_sasl_response :: reference()
	}).

init( ?args( AuthData ), SessionSrv ) ->
	AuthZID = proplists:get_value( auth_zid, AuthData, <<>> ),
	AuthCID = proplists:get_value( auth_cid, AuthData, <<>> ),
	AuthHost = proplists:get_value( auth_host, AuthData, <<>> ),
	AuthSecret = proplists:get_value( auth_secret, AuthData, <<>> ),
	ok = owl_session_service:notify_facility_available( auth_zid, ok, AuthZID ),
	ok = owl_session_service:notify_facility_available( auth_cid, ok, AuthCID ),
	ok = owl_session_service:notify_facility_available( auth_host, ok, AuthCID ),
	ok = owl_session_service:notify_facility_available( auth_secret, ok, AuthSecret ),
	{ok, #s{
			session_srv = SessionSrv,

			auth_zid = AuthZID,
			auth_cid = AuthCID,
			auth_host = AuthHost,
			auth_secret = AuthSecret
		}}.

prerequisites_available( S0 = #s{} ) ->
	{ok, S1} = do_subscribe_to_sasl_response( S0 ),
	{ok, _S2} = do_send_sasl_auth_request( S1 ).

do_subscribe_to_sasl_response( S0 = #s{ session_srv = SessionSrv } ) ->
	{ok, SubsID_SaslStanzaRef} = owl_session:subscribe( SessionSrv, ?match_sasl_auth_response, self(), ?prio_normal, infinity ),
	S1 = S0 #s{ subs_id_sasl_response = SubsID_SaslStanzaRef },
	{ok, S1}.

do_send_sasl_auth_request( S0 = #s{ session_srv = SessionSrv, auth_zid = AuthZID, auth_cid = AuthCID, auth_secret = AuthSecret } ) ->
	PlainCreds = [ AuthZID, AuthCID, AuthSecret ],
	SaslRequest = owl_stanza_sasl:stanza_new( <<"auth">>, [{<<"mechanism">>, <<"PLAIN">>}], PlainCreds ),
	ok = owl_session:send_stanza( SessionSrv, SaslRequest ),
	{ok, S0}.

query_facility( auth_zid, S = #s{ auth_zid = V } ) -> {ok, V, S};
query_facility( auth_cid, S = #s{ auth_cid = V } ) -> {ok, V, S};
query_facility( auth_host, S = #s{ auth_host = V } ) -> {ok, V, S};
query_facility( auth_secret, S = #s{ auth_secret = V } ) -> {ok, V, S};
query_facility( is_authenticated, S = #s{ is_authenticated = V } ) -> {ok, V, S}.


handle_call( Unexpected, ReplyTo, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_call, {unexpected_request, Unexpected}, {reply_to, ReplyTo} ]),
	{reply, badarg, S}.

handle_cast( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_cast, {unexpected_request, Unexpected} ]),
	{noreply, S}.


handle_info( ?xmpp_session_event( SessionSrv, SubsID, stanza, AuthResponse ), S = #s{ session_srv = SessionSrv, subs_id_sasl_response = SubsID } ) ->
	handle_info_stanza_sasl_response( AuthResponse, S );

handle_info( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_info, {unexpected_request, Unexpected} ]),
	{noreply, S}.

fun_any_of_mechs_available( MechNames ) ->
	fun( MechanismsFeature ) ->
		lists:any(
			fun( MechName ) ->
				owl_stanza_sasl:feature_mechanisms_has_mech( MechName, MechanismsFeature )
			end,
			MechNames )
	end.


handle_info_stanza_sasl_response( AuthResponse, S0 = #s{} ) ->
	case owl_stanza_sasl:parse_stanza( AuthResponse ) of
		{ok, <<"success">>, _, _} ->
			S1 = S0 #s{ is_authenticated = true },
			{ok, S2} = do_require_stream_restart( S1 ),
			ok = owl_session_service:notify_facility_available( is_authenticated, ok, true ),
			{noreply, S2};
		{ok, <<"failure">>, _, Reason} ->
			ok = owl_session_service:notify_facility_available( is_authenticated, error, Reason ),
			{noreply, S0}
	end.


do_require_stream_restart( S0 = #s{ session_srv = SessionSrv, auth_host = AuthHost } ) ->
	{ok, StreamSrv} = owl_session:get_stream( SessionSrv ),
	ok = owl_stream_tcp:send_stream_open( StreamSrv, [ {<<"to">>, AuthHost} ] ),
	{ok, S0}.
	% owl_session:require_stream_restart( SessionSrv ),
