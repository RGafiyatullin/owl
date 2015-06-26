-module (owl_session_srv).
-behaviour (gen_server).

-export ([
		start_link/2
	]).

-export ([
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		code_change/3,
		terminate/2
	]).

-include("log.hrl").
-include("owl_session_internals.hrl").
-include("xmpp_stream_events.hrl").
-include("xmpp_session_events.hrl").

-define( hib_timeout( S ), S #s.hib_timeout ).

start_link( StreamSrv, Args ) ->
	gen_server:start_link( ?MODULE, {srv, StreamSrv, Args}, [] ).

-define(hm, owl_session_handler_map).
-type handler_map() :: ?hm:handler_map().
-record( s, {
		'#module' = ?MODULE :: ?MODULE,

		stream_srv :: pid(),
		handler_map :: handler_map(),
		sats = dict:new() :: dict:dict( term(), pid() ),
		service_sup :: pid(),

		hib_timeout = 5000 :: timeout()
	} ).

init( { srv, StreamSrv, _Args } ) when is_pid( StreamSrv ) ->
	{ok, HM} = ?hm:new(),
	{ok, ServiceSup} = supervisor:start_link( ?MODULE, service_sup ),
	S0 = #s{
			stream_srv = StreamSrv,
			handler_map = HM,
			service_sup = ServiceSup
		},

	SInitialized = S0,
	{ok, SInitialized, ?hib_timeout( SInitialized )};
init( service_sup ) ->
	{ok, { {one_for_all, 0, 1}, [] }}.

handle_call( ?get_services(), ReplyTo, S0 ) ->
	handle_call_get_services( ReplyTo, S0 );

handle_call( ?get_stream, ReplyTo, S0 ) ->
	handle_call_get_stream( ReplyTo, S0 );

handle_call( ?send_stanza( Stanza ), ReplyTo, S0 ) ->
	handle_call_send_stanza( Stanza, ReplyTo, S0 );

handle_call( ?subscribe( MatchSpec, Predicates, HandlerPid, Priority, TriggerTimeout ), ReplyTo, S0 ) ->
	handle_call_subscribe( MatchSpec, Predicates, HandlerPid, Priority, TriggerTimeout, ReplyTo, S0 );

handle_call( ?unsubscribe( HandlerID ), ReplyTo, S0 ) ->
	handle_call_unsubscribe( HandlerID, ReplyTo, S0 );

handle_call( ?register_service( SatID, SatPid ), ReplyTo, S0 ) ->
	handle_call_register_service( SatID, SatPid, ReplyTo, S0 );

handle_call( ?get_service( SatID ), ReplyTo, S0 ) ->
	handle_call_get_service( SatID, ReplyTo, S0 );

handle_call( ?add_service( ServiceID, ServiceMod, ServiceArgs ), ReplyTo, S0 ) ->
	handle_call_add_service( ServiceID, ServiceMod, ServiceArgs, ReplyTo, S0 );

handle_call( Unexpected, ReplyTo, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_call, {unexpected_request, Unexpected}, {reply_to, ReplyTo} ]),
	{reply, badarg, S, ?hib_timeout( S )}.

handle_cast( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_cast, {unexpected_request, Unexpected} ]),
	{noreply, S, ?hib_timeout( S )}.

handle_info( timeout, S ) ->
	?log(debug, [ ?MODULE, handle_info_timeout, hibernating ]),
	{noreply, S, hibernate};

handle_info( ?xmpp_control_handed_over( StreamSrv ), S = #s{ stream_srv = StreamSrv } ) ->
	handle_info_xmpp_control_handed_over( S );

handle_info( ?xmpp_stream_open( StreamSrv, StreamAttrs ), S = #s{ stream_srv = StreamSrv } ) ->
	handle_info_xmpp_stream_open( StreamAttrs, S );

handle_info( ?xmpp_stanza( StreamSrv, Stanza ), S = #s{ stream_srv = StreamSrv } ) ->
	handle_info_xmpp_stanza( Stanza, S );

handle_info( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_info, {unexpected_request, Unexpected} ]),
	{noreply, S, ?hib_timeout( S )}.

code_change( OldVsn, Info, S ) ->
	?log(info, [?MODULE, code_change, {old_vsn, OldVsn}, {info, Info}]),
	{ok, S}.

terminate( _Reason, _S ) ->
	ignore.


handle_call_get_services( _ReplyTo, S0 = #s{ service_sup = ServiceSup } ) ->
	AllServices = [ {ID, P} || {ID, P, _, _} <- supervisor:which_children( ServiceSup ) ],
	{reply, {ok, AllServices}, S0, ?hib_timeout( S0 )}.


handle_call_get_stream( _ReplyTo, S0 = #s{ stream_srv = StreamSrv } ) ->
	{reply, {ok, StreamSrv}, S0}.

handle_call_register_service( SatID, SatPid, _ReplyTo, S0 = #s{ sats = Sats0 } ) ->
	case dict:find( SatID, Sats0 ) of
		{ok, ExistingSat} -> {reply, {error, {conflict, ExistingSat}}, S0, ?hib_timeout( S0 )};
		error ->
			Sats1 = dict:store( SatID, SatPid, Sats0 ),
			S1 = S0 #s{ sats = Sats1 },
			true = erlang:link( SatPid ),
			{reply, ok, S1, ?hib_timeout(S1)}
	end.

handle_call_add_service( ServiceID, ServiceMod, ServiceArgs, ReplyTo, S0 = #s{ service_sup = ServiceSup } ) ->
	ChildSpec = {
		ServiceID,
		{owl_session_service, start_link, [ self(), ServiceID, ServiceMod, ServiceArgs ]},
		permanent, 1000, worker, [ owl_session_service, ServiceMod ] },
	_ = proc_lib:spawn_link(fun() ->
			Result = supervisor:start_child(ServiceSup, ChildSpec),
			_ = gen_server:reply( ReplyTo, Result )
		end),
	{noreply, S0, ?hib_timeout(S0)}.


handle_call_get_service( SatID, _ReplyTo, S0 = #s{ sats = Sats } ) ->
	{reply, dict:find( SatID, Sats ), S0, ?hib_timeout(S0)}.


handle_call_send_stanza( Stanza, _ReplyTo, S0 = #s{ stream_srv = StreamSrv } ) ->
	ok = owl_stream_tcp:send_stanza( StreamSrv, Stanza ),
	{reply, ok, S0}.


handle_call_subscribe( MatchSpec, Predicates, HandlerPid, Priority, TriggerTimeout, _ReplyTo, S0 = #s{ handler_map = HM0 } ) ->
	{ok, HandlerID, HM1} = ?hm:subscribe( MatchSpec, Predicates, HandlerPid, Priority, TriggerTimeout, HM0 ),
	S1 = S0 #s{ handler_map = HM1 },

	{reply, {ok, HandlerID}, S1, ?hib_timeout( S1 )}.

handle_info_xmpp_control_handed_over( S = #s{ stream_srv = StreamSrv }) ->
	?log( debug, [ ?MODULE, handle_info_xmpp_control_handed_over ] ),
	ok = owl_stream_tcp:set_active( StreamSrv, once ),
	{noreply, S, ?hib_timeout( S )}.

handle_info_xmpp_stream_open( StreamAttrs, S = #s{ stream_srv = StreamSrv } ) ->
	?log( debug, [ ?MODULE, handle_info_xmpp_stream_open, {attrs, StreamAttrs} ] ),
	ok = owl_stream_tcp:set_active( StreamSrv, once ),
	{noreply, S, ?hib_timeout( S )}.

handle_info_xmpp_stanza( Stanza, S0 = #s{ stream_srv = StreamSrv, handler_map = HM0 } ) ->
	?log( debug, [ ?MODULE, handle_info_xmpp_stanza, {stanza, Stanza} ] ),

	StanzaProps = hm_get_stanza_props( Stanza ),
	{ok, Recepients, HM1} = ?hm:get_recepients( StanzaProps, HM0 ),

	ok = lists:foreach(
		fun( {RecepientPid, SubsID} ) ->
			Event = ?xmpp_session_event( self(), SubsID, stanza, Stanza ),
			_ = erlang:send( RecepientPid, Event )
		end,
		Recepients ),

	ok = owl_stream_tcp:set_active( StreamSrv, once ),
	S1 = S0 #s{ handler_map = HM1 },

	{noreply, S1, ?hib_timeout( S1 )}.

handle_call_unsubscribe( HandlerID, _ReplyTo, S0 = #s{ handler_map = HM0 } ) ->
	{ok, HM1} = ?hm:unsubscribe( HandlerID, HM0 ),
	S1 = S0 #s{ handler_map = HM1 },
	{reply, ok, S1}.


hm_get_stanza_props( Stanza ) ->
	{NS, NCN} = exp_node:fqn( Stanza ),
	ID = owl_stanza:id( Stanza ),
	{NS, NCN, ID, Stanza}.


