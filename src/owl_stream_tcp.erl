-module (owl_stream_tcp).
-compile ({parse_transform, gin}).
-export ([
		start_link/4, start_link/5,

		send_stream_open/2, send_stream_open/3,
		send_stanza/2, send_stanza/3,
		set_active/2, set_active/3,
		send_stream_close/1, send_stream_close/2,

		receive_xmpp_event/1, receive_xmpp_event/2,

		controlling_process/2, controlling_process/3
	]).

-include_lib("expellee/include/xml.hrl").

-include("owl_stream_tcp_internals.hrl").
-include("xmpp_stream_events.hrl").

-define( gd_is_srv( OwlTcpSrv ), ( is_pid( OwlTcpSrv ) orelse is_atom( OwlTcpSrv ) ) ).
-define( gd_is_port_number( Port ), ( is_integer( Port ) andalso Port >= 0 andalso Port < 65536 ) ).
-define( gd_is_activeness_mode( Mode ), in( Mode, [ true, false, once ] ) ).

-define( send_stream_open_timeout, 5000 ).
-define( send_stanza_timeout, 5000 ).
-define( set_active_timeout, 5000 ).
-define( receive_xmpp_event_timeout, 5000 ).
-define( send_stream_close_timeout, 5000 ).
-define( controlling_process_timeout, 5000 ).

start_link( tcp_connect, Host, Port, XmppTcpOpts ) ->
	start_link( tcp_connect, Host, Port, [], XmppTcpOpts ).

start_link( tcp_connect, Host, Port, TcpOpts, XmppTcpOpts )
	when is_list( TcpOpts )
	andalso is_list( XmppTcpOpts )
	andalso ?gd_is_port_number( Port )
->
	ControllingProcess = proplists:get_value( controlling_process, XmppTcpOpts, self() ),
	owl_stream_tcp_srv:start_link( ControllingProcess, {tcp_endpoint, Host, Port, TcpOpts}, XmppTcpOpts ).

receive_xmpp_event( Xmpp ) ->
	receive_xmpp_event( Xmpp, ?receive_xmpp_event_timeout ).
receive_xmpp_event( Xmpp, Timeout ) ->
	receive
		Event = ?xmpp_closed( Xmpp ) -> {ok, Event};
		Event = ?xmpp_socket_error( Xmpp, _ ) -> {ok, Event};
		Event = ?xmpp_error( Xmpp, _, _, _, _ ) -> {ok, Event};
		Event = ?xmpp_stream_open( Xmpp, _ ) -> {ok, Event};
		Event = ?xmpp_stream_close( Xmpp ) -> {ok, Event};
		Event = ?xmpp_stanza( Xmpp, _ ) -> {ok, Event}
	after
		Timeout -> {error, timeout}
	end.


controlling_process( OwlTcpSrv, NewControllingProcess ) ->
	controlling_process( OwlTcpSrv, NewControllingProcess, ?controlling_process_timeout ).
controlling_process( OwlTcpSrv, NewControllingProcess, Timeout ) when ?gd_is_srv( OwlTcpSrv ) andalso is_pid( NewControllingProcess ) ->
	OldControllingProcess = self(),
	gen_server:call( OwlTcpSrv, ?controlling_process( OldControllingProcess, NewControllingProcess ), Timeout ).

send_stream_open( OwlTcpSrv, StreamAttrs ) ->
	send_stream_open( OwlTcpSrv, StreamAttrs, ?send_stream_open_timeout ).
send_stream_open( OwlTcpSrv, StreamAttrs, Timeout ) when ?gd_is_srv( OwlTcpSrv ) ->
	ok = exp_check:check_attr_collection( StreamAttrs ),
	gen_server:call( OwlTcpSrv, ?send_stream_open( StreamAttrs ), Timeout ).


send_stanza( OwlTcpSrv, Stanza ) ->
	send_stanza( OwlTcpSrv, Stanza, ?send_stanza_timeout ).
send_stanza( OwlTcpSrv, Stanza, Timeout )
	when ?gd_is_srv( OwlTcpSrv )
	andalso ?is_xml_element( Stanza )
->
	gen_server:call( OwlTcpSrv, ?send_stanza( Stanza ), Timeout ).


set_active( OwlTcpSrv, Mode ) ->
	set_active( OwlTcpSrv, Mode, ?set_active_timeout ).
set_active( OwlTcpSrv, Mode, Timeout ) when ?gd_is_srv( OwlTcpSrv ) andalso ?gd_is_activeness_mode( Mode ) ->
	gen_server:call( OwlTcpSrv, ?set_active( Mode ), Timeout ).


send_stream_close( OwlTcpSrv ) ->
	send_stream_close( OwlTcpSrv, ?send_stream_close_timeout ).
send_stream_close( OwlTcpSrv, Timeout ) when ?gd_is_srv( OwlTcpSrv ) ->
	gen_server:call( OwlTcpSrv, ?send_stream_close(), Timeout ).


