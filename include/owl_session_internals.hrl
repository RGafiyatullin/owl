-ifndef( owl_include_owl_session_iternals_hrl ).
-define( owl_include_owl_session_iternals_hrl, true ).

-define( subscribe( MatchSpec, Predicates, HandlerPid, Priority, TriggerTimeout ),
			{subscribe, MatchSpec, Predicates, HandlerPid, Priority, TriggerTimeout} ).
-define( unsubscribe( HandlerID ), {unsubscribe, HandlerID} ).

-define( add_service( ServiceID, ServiceMod, ServiceArgs ), {add_service, ServiceID, ServiceMod, ServiceArgs} ).
-define( register_service( SvcID, SvcPid ), { register_satellite, SvcID, SvcPid } ).
-define( get_service( SvcID ), {get_satellite, SvcID} ).
-define( get_services(), get_services ).

-define( get_stream, get_stream ).
-define( send_stanza( Stanza ), {send_stanza, Stanza} ).

-endif. % owl_include_owl_session_iternals_hrl
