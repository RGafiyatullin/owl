-module (owl_xmpp_session_service).
-compile ({parse_transform, gin}).
-behaviour (gen_server).


-export ([
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		code_change/3,
		terminate/2
	]).

-include_lib("expellee/include/xml.hrl").
-include("log.hrl").
-include("xmpp_session_events.hrl").
-include("const.hrl").
-include("ns_xmpp_core.hrl").

-define( _log( Lvl, Report, S ), ?log( Lvl, [ {service, {S #s.session_srv, S#s.service_id}} | Report ] ) ).

%% Exo-API
-export ([
		start_link/4,
		launch/1,
		query_facility/3,
		query_facility/4,
		async_enquire_facility/5
	]).

%% Endo-API
-export ([
		notify_facility_available/3
	]).

-type gen_reply_to() :: {pid(), reference()}.

-type service_mod() :: atom().
-type service_id() :: term().
-type service_facility_id() :: term().
-type service_args() :: term().
-type service_state() :: term().
-type facility_data() :: term().

-type fun_xml_element_to_boolean() :: fun( (xml_element()) -> boolean()).

-type stream_feature_spec() ::
	{NS :: xml_ns(), NCN :: xml_ncname(), Predicates :: [fun_xml_element_to_boolean()]}.
-type service_facility_requirement_spec() ::
	{ service_id(), service_facility_id(), timeout() }.

-callback stream_features_required( service_args() ) -> {ok, [ stream_feature_spec() ]}.
-callback facilities_required( service_args() ) -> {ok, [ service_facility_requirement_spec() ]}.
-callback facilities_provided( service_args() ) -> {ok, [ service_facility_id() ]}.
-callback init( service_args(), SessionSrv :: owl_xmpp:session_srv() ) -> {ok, service_state()}.
-callback query_facility( service_facility_id(), service_state() ) -> {ok, facility_data(), service_state()}.
-callback prerequisites_available( service_state() ) -> {ok, service_state()}.
-callback handle_call( Call :: term(), ReplyTo :: gen_reply_to(), service_state() ) -> {noreply, service_state()} | {reply, Reply :: term(), service_state()} | {stop, Reason :: term(), Reply :: term(), service_state()}.
-callback handle_cast( Cast :: term(), service_state() ) -> {noreply, service_state()} | {stop, Reason :: term(), service_state()}.
-callback handle_info( Info :: term(), service_state() ) -> {noreply, service_state()} | {stop, Reason :: term(), service_state()}.



start_link( SessionSrv, ServiceID, ServiceMod, ServiceArgs ) ->
	gen_server:start_link( ?MODULE, {SessionSrv, ServiceID, ServiceMod, ServiceArgs}, [] ).

launch( Service ) when is_pid( Service ) ->
	gen_server:call( Service, {?MODULE, launch} ).

-define(required_facility_report( EnquiryPid, ServiceID, FacilityID, ResultType, ResultData ),
		{?MODULE, required_facility_report, EnquiryPid, ServiceID, FacilityID, ResultType, ResultData } ).

async_enquire_facility( ReportTo, ServicePid, ServiceID, FacilityID, Timeout ) ->
	?log(trace, [ ?MODULE, async_enquire_facility, {for, ReportTo}, {facility, {ServiceID, FacilityID}}, {timeout, Timeout} ]),
	try
		case ?MODULE:query_facility( ServicePid, FacilityID, Timeout ) of
			{ok, FacilityData} ->
				?log(trace, [ ?MODULE, async_enquire_facility, {for, ReportTo}, {facility, {ServiceID, FacilityID}}, {ok, FacilityData} ]),
				gen_server:cast( ReportTo, ?required_facility_report( self(), ServiceID, FacilityID, ok, FacilityData ) );
			{error, Reason} ->
				error(Reason);
			Unexpected ->
				error({unexpected_ret_value, Unexpected})
		end
	catch
		CaughtError:CaughtReason ->
			?log(trace, [ ?MODULE, async_enquire_facility, {for, ReportTo}, {facility, {ServiceID, FacilityID}}, {CaughtError, CaughtReason} ]),
			gen_server:cast( ReportTo, ?required_facility_report( self(), ServiceID, FacilityID, error, {CaughtError, CaughtReason} ) )
	end.

query_facility( ServicePid, FacilityID, Timeout ) ->
	gen_server:call( ServicePid, {?MODULE, query_facility, FacilityID}, Timeout ).

query_facility( SessionSrv, ServiceID, FacilityID, Timeout ) ->
	case owl_xmpp_session:get_service( SessionSrv, ServiceID ) of
		{ok, ServicePid} -> query_facility( ServicePid, FacilityID, Timeout );
		Error -> Error
	end.

notify_facility_available( FacilityID, OkayOrError, FacilityData ) ->
	ok = gen_server:cast( self(), {?MODULE, facility_available, FacilityID, OkayOrError, FacilityData} ).



-define( hib_timeout(S), ( S#s.hib_timeout )).
-record(s, {
		'#module' = ?MODULE :: ?MODULE,
		hib_timeout = 5000 :: timeout(),

		session_srv :: owl_xmpp:session_srv(),

		service_id :: service_id(),
		service_mod :: service_mod(),
		service_args :: service_args(),
		service_state :: undefined | service_state(),

		stream_features_subs_id :: undefined | reference(),
		stream_features_required = [] :: [ stream_feature_spec() ],
		stream_features_satisfy_requirements = false :: boolean(),

		service_facilities_required = [] :: [ service_facility_requirement_spec() ],
		service_facilities_satisfy_requirements = false :: boolean(),
		service_facility_enquiry_pids = sets:new() :: sets:set( pid() ),

		service_facilities_provided = dict:new() :: dict:dict( service_facility_id(), boolean() ),
		service_facilities_provided_pending_queries = dict:new() :: dict:dict( service_facility_id(), [ gen_reply_to() ] )
	}).

-spec init({ owl_xmpp:session_srv(), service_id(), service_mod(), service_args() }) -> {ok, #s{}, timeout()}.
init( {SessionSrv, ServiceID, ServiceMod, ServiceArgs} ) ->
	S0 = #s{
			session_srv = SessionSrv,
			service_id = ServiceID,
			service_mod = ServiceMod,
			service_args = ServiceArgs
		},
	{ok, S1} = do_register_service( S0 ),
	{ok, S2} = do_init_stream_features_required( S1 ),
	{ok, S3} = do_init_facilities_required( S2 ),
	{ok, S4} = do_init_facilities_provided( S3 ),
	{ok, S5} = do_init_service_state( S4 ),
	SInitialized = S5,
	?_log(debug, [ ?MODULE, init, complete ], SInitialized),
	{ok, SInitialized, ?hib_timeout( SInitialized )}.

handle_call( {?MODULE, query_facility, FacilityID}, ReplyTo, S ) ->
	handle_call_query_facility( FacilityID, ReplyTo, S );

handle_call( {?MODULE, launch}, ReplyTo, S ) ->
	handle_call_launch( ReplyTo, S );

handle_call( Call, ReplyTo, S = #s{ service_mod = M, service_state = SubS0 } ) ->
	case M:handle_call( Call, ReplyTo, SubS0 ) of
		{reply, ReplyWith, SubS1} -> {reply, ReplyWith, S #s{ service_state = SubS1 }, ?hib_timeout( S )};
		{noreply, SubS1} -> {noreply, S #s{ service_state = SubS1 }, ?hib_timeout( S )};
		{stop, Reason, Reply, SubS1} -> {stop, Reason, Reply, S #s{ service_state = SubS1 }}
	end.

% handle_call( Unexpected, ReplyTo, S = #s{} ) ->
% 	?log(warning, [ ?MODULE, handle_call, {unexpected_request, Unexpected}, {reply_to, ReplyTo} ]),
% 	{reply, badarg, S, ?hib_timeout( S )}.

handle_cast( {?MODULE, check_prerequisites}, S ) ->
	handle_cast_check_prerequisites( S );

handle_cast( {?MODULE, facility_available, FacilityID, OkayOrError, FacilityData}, S ) ->
	handle_cast_notify_facility_available( FacilityID, OkayOrError, FacilityData, S );

handle_cast( ?required_facility_report( EnquiryPid, ServiceID, FacilityID, ResultType, ResultData ), S ) ->
	handle_cast_required_facility_report( EnquiryPid, ServiceID, FacilityID, ResultType, ResultData, S );

handle_cast( Cast, S = #s{ service_mod = M, service_state = SubS0 } ) ->
	case M:handle_cast( Cast, SubS0 ) of
		{noreply, SubS1} -> {noreply, S #s{ service_state = SubS1 }, ?hib_timeout( S )};
		{stop, Reason, SubS1} -> {stop, Reason, S #s{ service_state = SubS1 }}
	end.

% handle_cast( Unexpected, S = #s{} ) ->
% 	?log(warning, [ ?MODULE, handle_cast, {unexpected_request, Unexpected} ]),
% 	{noreply, S, ?hib_timeout( S )}.

handle_info(
		?xmpp_session_event( SessionSrv, SubsID, stanza, StreamFeaturesStanza ),
		S = #s{ stream_features_subs_id = SubsID, session_srv = SessionSrv }
	) ->
		handle_info_stream_features( StreamFeaturesStanza, S );

handle_info( timeout, S ) ->
	?_log(debug, [ ?MODULE, handle_info_timeout, hibernating ], S),
	{noreply, S, hibernate};

handle_info( Info, S = #s{ service_mod = M, service_state = SubS0 } ) ->
	case M:handle_info( Info, SubS0 ) of
		{noreply, SubS1} -> {noreply, S #s{ service_state = SubS1 }, ?hib_timeout( S )};
		{stop, Reason, SubS1} -> {stop, Reason, S #s{ service_state = SubS1 }}
	end.

% handle_info( Unexpected, S = #s{} ) ->
% 	?log(warning, [ ?MODULE, handle_info, {unexpected_request, Unexpected} ]),
% 	{noreply, S, ?hib_timeout( S )}.

code_change( OldVsn, Info, S ) ->
	?_log(info, [?MODULE, code_change, {old_vsn, OldVsn}, {info, Info}], S),
	{ok, S}.

terminate( _Reason, _S ) ->
	ignore.


handle_cast_notify_facility_available( FacilityID, OkayOrError, FacilityData, S0 = #s{ service_facilities_provided = FacilityStates0 } ) ->
	case dict:find( FacilityID, FacilityStates0 ) of
		error ->
			?_log(error, [?MODULE, handle_cast_notify_facility_available, {received_facility_notification_about_unexpected_facility_id, FacilityID}], S0),
			{noreply, S0, ?hib_timeout(S0)};
		{ok, _} ->
			?_log(debug, [?MODULE, handle_cast_notify_facility_available, {facility_id, FacilityID}, {facility_data, FacilityData}], S0),
			FacilityStates1 = dict:store( FacilityID, true, FacilityStates0 ),
			S1 = S0 #s{ service_facilities_provided = FacilityStates1 },
			{ok, S2} = do_reply_upon_facility_available( FacilityID, OkayOrError, FacilityData, S1 ),
			{noreply, S2, ?hib_timeout( S2 )}
	end.

handle_call_query_facility(
	FacilityID, ReplyTo,
	S0 = #s{
		service_mod = M,
		service_state = SubS0,
		service_facilities_provided = FacilityStates,
		service_facilities_provided_pending_queries = PendingQueries0
	}
) ->
	case dict:find( FacilityID, FacilityStates ) of
		error ->
			{reply, {error, service_unavailable}, S0, ?hib_timeout( S0 )};

		{ok, true} ->
			{ok, FacilityData, SubS1} = M:query_facility( FacilityID, SubS0 ),
			S1 = S0 #s{ service_state = SubS1 },
			{reply, {ok, FacilityData}, S1, ?hib_timeout( S1 )};

		{ok, false} ->
			QueriesForFacility =
				case dict:find( FacilityID, PendingQueries0 ) of
					{ok, Existing} -> Existing;
					error -> []
				end,
			PendingQueries1 = dict:store( FacilityID, [ ReplyTo | QueriesForFacility ], PendingQueries0 ),
			S1 = S0 #s{ service_facilities_provided_pending_queries = PendingQueries1 },
			{noreply, S1, ?hib_timeout( S1 )}
	end.

handle_call_launch( _ReplyTo, S0 ) ->
	?_log( debug, [ handle_call_launch ], S0 ),
	{ok, S1} = do_subscribe_to_stream_features( S0 ),
	{ok, S2} = do_enquire_service_facilities( S1 ),
	{reply, ok, S2, ?hib_timeout(S2)}.

handle_cast_required_facility_report( EnquiryPid, ServiceID, FacilityID, OkayOrError, ResultData, S0 = #s{ service_facility_enquiry_pids = EnquiryPidsSet0 } ) ->
	?_log( debug, [ ?MODULE, handle_cast_required_facility_report, {required_facility, {ServiceID, FacilityID}}, {result, {OkayOrError, ResultData}}], S0 ),
	case OkayOrError of
		ok ->
			EnquiryPidsSet1 = sets:del_element( EnquiryPid, EnquiryPidsSet0 ),
			S1 = S0 #s{ service_facility_enquiry_pids = EnquiryPidsSet1, service_facilities_satisfy_requirements = (sets:size( EnquiryPidsSet1 ) == 0) },
			{ok, S2} = do_cast_check_prerequisites( S1 ),
			{noreply, S2, ?hib_timeout( S2 )};
		error ->
			?_log( warning, [ ?MODULE, handle_cast_required_facility_report,
				{required_facility_failed, {ServiceID, FacilityID}}, {error, ResultData} ], S0 ),
			{stop, {required_facility_failed, {ServiceID, FacilityID}, ResultData}, S0}
	end.


handle_cast_check_prerequisites( S0 = #s{ stream_features_satisfy_requirements = true, service_facilities_satisfy_requirements = true } ) ->
	?_log(trace, [ ?MODULE, handle_cast_check_prerequisites, prerequisites_available ], S0),
	{ok, S1} = do_notify_prerequisites_available( S0 ),
	{noreply, S1, ?hib_timeout(S1)};
handle_cast_check_prerequisites( S0 = #s{} ) ->
	?_log(trace, [ ?MODULE, handle_cast_check_prerequisites, prerequisites_are_not_yet_available ], S0),
	{noreply, S0, ?hib_timeout(S0)}.

handle_info_stream_features( StreamFeaturesStanza, S0 = #s{ stream_features_required = FeatureSpecs }) ->
	?_log(trace, [ ?MODULE, handle_info_stream_features, {stream_features_stanza, StreamFeaturesStanza}], S0),
	FunCheckSpecIsSatisfied = fun( {NS, NCN, FeatureSpecPredicates} ) ->
			case owl_xmpp_stanza_stream_features:get_feature( NS, NCN, StreamFeaturesStanza ) of
				error -> false;

				{ok, Feature} ->
					_SatisfiesAllPredicates =
						lists:all(
							fun( FeatureSpecPredicate ) ->
								FeatureSpecPredicate( Feature )
							end,
							FeatureSpecPredicates )
			end
		end,
	StreamFeaturesOkay = lists:all( FunCheckSpecIsSatisfied, FeatureSpecs ),
	S1 = S0 #s{ stream_features_satisfy_requirements = StreamFeaturesOkay },
	{ok, S2} = do_cast_check_prerequisites( S1 ),
	{noreply, S2, ?hib_timeout( S2 )}.

do_cast_check_prerequisites( S0 ) ->
	ok = gen_server:cast( self(), {?MODULE, check_prerequisites} ),
	{ok, S0}.

do_init_stream_features_required( S0 = #s{ service_mod = M, service_args = ServiceArgs } ) ->
	?_log(trace, [?MODULE, do_init_stream_features_required], S0),
	{ok, FeatureSpecs} = M:stream_features_required( ServiceArgs ),
	S1 = S0 #s{ stream_features_required = FeatureSpecs },
	{ok, S1}.
do_init_facilities_required( S0 = #s{ service_mod = M, service_args = ServiceArgs } ) ->
	?_log(trace, [?MODULE, do_init_facilities_required], S0),
	{ok, FacilitySpecs} = M:facilities_required( ServiceArgs ),
	S1 = S0 #s{ service_facilities_required = FacilitySpecs },
	{ok, S1}.

do_init_service_state( S0 = #s{ session_srv = SessionSrv, service_mod = M, service_args = ServiceArgs } ) ->
	{ok, SubS} = M:init( ServiceArgs, SessionSrv ),
	S1 = S0 #s{ service_state = SubS },
	{ok, S1}.

do_enquire_service_facilities( S0 = #s{ session_srv = SessionSrv, service_facilities_required = RequiredServiceFacilitySpecs } ) ->
	RepliesExpectedFromPids = lists:map(
			fun ( {ServiceID, FacilityID, Timeout} ) ->
				case owl_xmpp_session:get_service( SessionSrv, ServiceID ) of
					error -> error( {required_service_missing, ServiceID} );
					{ok, ServicePid} ->
						_EnquiryPid = proc_lib:spawn_link( ?MODULE, async_enquire_facility, [ self(), ServicePid, ServiceID, FacilityID, Timeout ] )
				end
			end,
			RequiredServiceFacilitySpecs ),
	{ok, S0 #s{
			service_facility_enquiry_pids = sets:from_list( RepliesExpectedFromPids ),
			service_facilities_satisfy_requirements = (RequiredServiceFacilitySpecs == [])
		}}.

do_init_facilities_provided( S0 = #s{ service_mod = M, service_args = ServiceArgs } ) ->
	{ok, FacilitiesProvided} = M:facilities_provided( ServiceArgs ),
	ProvidedFacilityStates = dict:from_list([ {Facility, false} || Facility <- FacilitiesProvided ]),
	S1 = S0 #s{ service_facilities_provided = ProvidedFacilityStates },
	{ok, S1}.


do_reply_upon_facility_available(
		FacilityID, OkayOrError, FacilityData,
		S0 = #s{ service_facilities_provided_pending_queries = PendingQueries0 }
	) when in(OkayOrError, [ok, error])
->
	case dict:find( FacilityID, PendingQueries0 ) of
		error -> {ok, S0};
		{ok, ReplyToList} ->
			PendingQueries1 = dict:erase( FacilityID, PendingQueries0 ),
			S1 = S0 #s{ service_facilities_provided_pending_queries = PendingQueries1 },
			ok = lists:foreach(
				fun( ReplyTo ) ->
					_ = gen_server:reply( ReplyTo, {OkayOrError, FacilityData} )
				end, ReplyToList),
			{ok, S1}
	end.

do_register_service( S0 = #s{ session_srv = SessionSrv, service_id = ServiceID } ) ->
	ok = owl_xmpp_session:register_service( SessionSrv, ServiceID, self() ),
	{ok, S0}.

-define(match_stream_features, {fqn, ?ns_jabber_streams, <<"features">>}).
do_subscribe_to_stream_features( S0 = #s{
		session_srv = SessionSrv,
		stream_features_subs_id = undefined
	} ) ->
		{ok, SubsID_StreamFeatures} = owl_xmpp_session:subscribe(
				SessionSrv, ?match_stream_features,
				self(), ?prio_normal, infinity
			),
		S1 = S0 #s{ stream_features_subs_id = SubsID_StreamFeatures },
		{ok, S1}.

do_notify_prerequisites_available( S0 = #s{ service_mod = M, service_state = SubS0 } ) ->
	{ok, SubS1} = M:prerequisites_available( SubS0 ),
	S1 = S0 #s{ service_state = SubS1 },
	{ok, S1}.






