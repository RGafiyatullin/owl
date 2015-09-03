-ifndef( owl_xmpp_include_xmpp_session_events_hrl ).
-define( owl_xmpp_include_xmpp_session_events_hrl, true ).

-define( xmpp_session_event( SessionSrv, SubsID, Type, Payload ), { xmpp_session_event, SessionSrv, SubsID, Type, Payload } ).

-endif. % owl_xmpp_include_xmpp_session_events_hrl
