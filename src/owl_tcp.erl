-module (owl_tcp).
-compile ({parse_transform, gin}).
-export ([
		start_link/4, start_link/5,

		send_stream_open/2, send_stream_open/3,
		send_stanza/2, send_stanza/3,
		set_active/2, set_active/3,

		receive_xmpp_event/2, receive_xmpp_event/1
	]).

-include_lib("expellee/include/xml.hrl").

-include("owl_tcp_internals.hrl").
-include("xmpp_stream_events.hrl").

-define( gd_is_srv( XmppTcp ), ( is_pid( XmppTcp ) orelse is_atom( XmppTcp ) ) ).
-define( gd_is_port_number( Port ), ( is_integer( Port ) andalso Port >= 0 andalso Port < 65536 ) ).
-define( gd_is_activeness_mode( Mode ), in( Mode, [ true, false, once ] ) ).

-define( send_stream_open_timeout, 5000 ).
-define( send_stanza_timeout, 5000 ).
-define( set_active_timeout, 5000 ).
-define( receive_xmpp_event_timeout, 5000 ).

start_link( tcp_connect, Host, Port, XmppTcpOpts ) ->
	start_link( tcp_connect, Host, Port, [], XmppTcpOpts ).

start_link( tcp_connect, Host, Port, TcpOpts, XmppTcpOpts )
	when is_list( TcpOpts )
	andalso is_list( XmppTcpOpts )
	andalso ?gd_is_port_number( Port )
->
	ControllingProcess = proplists:get_value( controlling_process, XmppTcpOpts, self() ),
	owl_tcp_srv:start_link( ControllingProcess, {tcp_endpoint, Host, Port, TcpOpts}, XmppTcpOpts ).

receive_xmpp_event( Xmpp ) ->
	receive_xmpp_event( Xmpp, ?receive_xmpp_event_timeout ).
receive_xmpp_event( Xmpp, Timeout ) ->
	receive
% -define( xmpp_socket_error( Xmpp, SocketError ), {xmpp_socket_error, SocketError} ).
% -define( xmpp_error( Xmpp, CausingParty, IsTerminal, Type, Details ), {xmpp_error, CausingParty, IsTerminal, Type, Details} ).
% -define( xmpp_stream_open( Xmpp, Attrs ), {xmpp_stream_open, Xmpp, Attrs} ).
% -define( xmpp_stream_close( Xmpp ), {xmpp_stream_close, Xmpp} ).
% -define( xmpp_stanza( Xmpp, Stanza ), {xmpp_stanza, Xmpp, Stanza} ).
		Event = ?xmpp_socket_error( Xmpp, _ ) -> {ok, Event};
		Event = ?xmpp_error( Xmpp, _, _, _, _ ) -> {ok, Event};
		Event = ?xmpp_stream_open( Xmpp, _ ) -> {ok, Event};
		Event = ?xmpp_stream_close( Xmpp ) -> {ok, Event};
		Event = ?xmpp_stanza( Xmpp, _ ) -> {ok, Event}
	after
		Timeout -> {error, timeout}
	end.


send_stream_open( XmppTcp, StreamAttrs ) ->
	send_stream_open( XmppTcp, StreamAttrs, ?send_stream_open_timeout ).
send_stream_open( XmppTcp, StreamAttrs, Timeout ) when ?gd_is_srv( XmppTcp ) ->
	ok = exp_check:check_attr_collection( StreamAttrs ),
	gen_server:call( XmppTcp, ?send_stream_open( StreamAttrs ), Timeout ).


send_stanza( XmppTcp, Stanza ) ->
	send_stanza( XmppTcp, Stanza, ?send_stanza_timeout ).
send_stanza( XmppTcp, Stanza, Timeout )
	when ?gd_is_srv( XmppTcp )
	andalso ?is_xml_element( Stanza )
->
	gen_server:call( XmppTcp, ?send_stanza( Stanza ), Timeout ).


set_active( XmppTcp, Mode ) ->
	set_active( XmppTcp, Mode, ?set_active_timeout ).
set_active( XmppTcp, Mode, Timeout ) when ?gd_is_srv( XmppTcp ) andalso ?gd_is_activeness_mode( Mode ) ->
	gen_server:call( XmppTcp, ?set_active( Mode ), Timeout ).





