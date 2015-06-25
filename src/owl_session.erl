-module (owl_session).
-compile ({parse_transform, gin}).
-export ([
		start_link/2,
		subscribe/5,
		unsubscribe/2,

		register_service/3,
		add_service/4,
		launch_services/1,

		get_service/2,
		get_stream/1,
		send_stanza/2
	]).

-include_lib("expellee/include/xml.hrl").
-include("owl_session_internals.hrl").
-include("xmpp_session_events.hrl").

-define( gd_is_srv(Srv), (is_pid( Srv ) orelse is_atom( Srv )) ).
-define( gd_is_timeout( T ), (( is_integer( T ) andalso T >= 0 ) orelse T == infinity) ).

-define( subscribe_call_timeout, 5000 ).
-define( unsubscribe_call_timeout, 5000 ).
-define( register_service_call_timeout, 5000 ).
-define( get_service_call_timeout, 5000 ).
-define( get_stream_call_timeout, 5000 ).
-define( send_stanza_call_timeout, 5000 ).

-type owl_session_arg() :: tuple().

-type handler_priority() :: integer().

-type handler_match_spec() :: owl_session_handler_map:match_spec().

-spec start_link( OwlStreamSrv :: pid(), Args :: [ owl_session_arg() ] ) -> {ok, SessionSrv :: pid()}.

-spec subscribe(
		SessionSrv :: pid(), MatchSpec :: handler_match_spec(), HandlerPid :: pid(),
		Priority :: handler_priority(), TriggerTimeout :: timeout()
	) -> {ok, HandlerID :: reference()}.

-spec unsubscribe( SessionSrv :: pid(), HandlerID :: reference() ) -> ok.

-spec register_service( SessionSrv :: pid(), SatID :: term(), SatPid :: pid() ) -> ok.

-spec get_service( SessionSrv :: pid(), SatID :: term() ) -> {ok, SatPid :: pid()} | error.

-spec get_stream( SessionSrv :: pid() ) -> {ok, StreamSrv :: pid()}.

-spec send_stanza( SessionsSrv :: pid(), Stanza :: xml_element() ) -> ok.

start_link( StreamSrv, Args ) ->
	owl_session_srv:start_link( StreamSrv, Args ).

subscribe( SessionSrv, MatchSpec, HandlerPid, Priority, TriggerTimeout )
	when ?gd_is_srv( SessionSrv )
	andalso is_pid( HandlerPid )
	andalso is_integer( Priority )
	andalso ?gd_is_timeout( TriggerTimeout )
->
	Request = ?subscribe( MatchSpec, HandlerPid, Priority, TriggerTimeout ),
	gen_server:call( SessionSrv, Request, ?subscribe_call_timeout ).

unsubscribe( SessionSrv, HandlerID ) when ?gd_is_srv( SessionSrv ) andalso is_reference( HandlerID ) ->
	Request = ?unsubscribe( HandlerID ),
	ok = gen_server:call( SessionSrv, Request, ?unsubscribe_call_timeout ),
	receive
		?xmpp_session_event( SessionSrv, HandlerID, _, _ ) -> ok
	after
		0 -> ok
	end.

launch_services( SessionSrv ) ->
	{ok, AllServices} = gen_server:call( SessionSrv, ?get_services() ),
	ok = lists:foreach(
		fun({_ID, Pid}) ->
			ok = owl_session_service:launch( Pid )
		end,
		AllServices).

add_service( SessionSrv, ServiceID, ServiceMod, ServiceArgs ) ->
	gen_server:call( SessionSrv, ?add_service( ServiceID, ServiceMod, ServiceArgs ) ).

register_service( SessionSrv, SatID, SatPid ) when ?gd_is_srv( SessionSrv ) andalso is_pid( SatPid ) ->
	Request = ?register_service( SatID, SatPid ),
	gen_server:call( SessionSrv, Request, ?register_service_call_timeout ).

get_service( SessionSrv, SatID ) when ?gd_is_srv( SessionSrv ) ->
	Request = ?get_service( SatID ),
	gen_server:call( SessionSrv, Request, ?get_service_call_timeout ).

get_stream( SessionSrv ) when ?gd_is_srv( SessionSrv ) ->
	gen_server:call( SessionSrv, ?get_stream, ?get_stream_call_timeout ).

send_stanza( SessionSrv, Stanza ) when ?gd_is_srv( SessionSrv ) andalso ?is_xml_xe( Stanza ) ->
	Request = ?send_stanza( Stanza ),
	gen_server:call( SessionSrv, Request, ?send_stanza_call_timeout ).

