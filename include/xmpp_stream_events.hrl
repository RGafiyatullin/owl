-ifndef(owl_include_xmpp_stream_events_hrl).
-define(owl_include_xmpp_stream_events_hrl, true).

-include_lib ("expellee/include/xml.hrl").

-define( xmpp_closed( Xmpp ), { xmpp_closed, Xmpp } ).
-define( xmpp_socket_error( Xmpp, SocketError ), {xmpp_socket_error, SocketError} ).
-define( xmpp_error( Xmpp, CausingParty, IsTerminal, Type, Details ), {xmpp_error, CausingParty, IsTerminal, Type, Details} ).
-define( xmpp_stream_open( Xmpp, Attrs ), {xmpp_stream_open, Xmpp, Attrs} ).
-define( xmpp_stream_close( Xmpp ), {xmpp_stream_close, Xmpp} ).
-define( xmpp_stanza( Xmpp, Stanza ), {xmpp_stanza, Xmpp, Stanza} ).

-define( xmpp_control_handed_over( Xmpp ), {xmpp_control_handed_over, Xmpp} ).

-type xmpp_stream_event() ::
	  ?xmpp_closed( pid() )
	| ?xmpp_socket_error( pid(), atom() )
	| ?xmpp_error( pid(), peer | self, boolean(), term(), term() )
	| ?xmpp_stream_open( pid(), [ xml_attribute() ] )
	| ?xmpp_stream_close( pid() )
	| ?xmpp_stanza( pid(), xml_element() ).

-endif. % owl_include_xmpp_stream_events_hrl
