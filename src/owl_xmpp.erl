-module(owl_xmpp).
-owl_mod(owl_xmpp).

-include("xmpp_error_common.hrl").

-include("xmpp_stanza_error.hrl").
-include("xmpp_stream_error.hrl").

-include("xmpp_session_events.hrl").
-include("xmpp_stream_events.hrl").

-include_lib("expellee/include/xml.hrl").


%% Error types
-type xmpp_error_common() :: #xmpp_error_common{}.
-type xmpp_stanza_error_pub() :: #xmpp_stanza_error_pub{}.
-type xmpp_stanza_error() :: #xmpp_stanza_error{}.
-type xmpp_stream_error_pub() :: #xmpp_stream_error_pub{}.
-type xmpp_stream_error() :: #xmpp_stream_error{}.


%% Session events
-type session_srv() :: pid().
-type xmpp_session_event() :: {
	xmpp_session_event,
	SessionSrv :: session_srv(),
	SubsID :: reference(),
	Type :: atom(),
	Payload :: term()
}.


%% Stream events
-type xmpp_stream_tcp_srv() :: pid().
-type xmpp_stream_socket_error() :: atom().
-type xmpp_stream_error_causing_party() :: peer | self.
-type xmpp_stream_error_is_terminal() :: boolean().
-type xmpp_stream_error_type() :: term().
-type xmpp_stream_error_details() :: [term()].
-type xmpp_stream_closed_event() :: {
	xmpp_closed,
	TcpSrv :: xmpp_stream_tcp_srv()
}.
-type xmpp_stream_socket_error_event() :: {
	xmpp_socket_error,
	TcpSrv :: xmpp_stream_tcp_srv(),
	SocketError :: xmpp_stream_socket_error()
}.
-type xmpp_stream_error_event() :: {
	xmpp_error,
	TcpSrv :: xmpp_stream_tcp_srv(),
	CausingParty :: xmpp_stream_error_causing_party(),
	IsTerminal :: xmpp_stream_error_is_terminal(),
	Type :: xmpp_stream_error_type(),
	Details :: xmpp_stream_error_details()
}.
-type xmpp_stream_open_event() :: {
	xmpp_stream_open,
	TcpSrv :: xmpp_stream_tcp_srv(),
	Attrs :: [xml_attribute()]
}.
-type xmpp_stream_close_event() :: {
	xmpp_stream_open,
	TcpSrv :: xmpp_stream_tcp_srv()
}.
-type xmpp_stream_stanza_event() :: {
	xmpp_stanza,
	TcpSrv :: xmpp_stream_tcp_srv(),
	Stanza :: xml_element()
}.
-type xmpp_stream_control_handed_over_event() :: {
	xmpp_control_handed_over,
	TcpSrv :: xmpp_stream_tcp_srv()
}.
-type xmpp_stream_event() ::
	xmpp_stream_closed_event() |
	xmpp_stream_socket_error_event() |
	xmpp_stream_error_event() |
	xmpp_stream_open_event() |
	xmpp_stream_close_event() |
	xmpp_stream_stanza_event() |
	xmpp_stream_control_handed_over_event().


%% Exports
-export_type([
	xmpp_stanza_error_condition/0,
	xmpp_stream_error_condition/0,
	xmpp_error_common/0,
	xmpp_stanza_error_pub/0,
	xmpp_stanza_error/0,
	xmpp_stream_error_pub/0,
	xmpp_stream_error/0,

	session_srv/0,
	xmpp_session_event/0,

	xmpp_stream_tcp_srv/0,
	xmpp_stream_socket_error/0,
	xmpp_stream_error_causing_party/0,
	xmpp_stream_error_is_terminal/0,
	xmpp_stream_error_type/0,
	xmpp_stream_error_details/0,
	xmpp_stream_closed_event/0,
	xmpp_stream_socket_error_event/0,
	xmpp_stream_error_event/0,
	xmpp_stream_open_event/0,
	xmpp_stream_close_event/0,
	xmpp_stream_stanza_event/0,
	xmpp_stream_control_handed_over_event/0,
	xmpp_stream_event/0
]).


%% expellee types
-export_type([
	xml_ncname/0,
	xml_ns/0,
	xml_name/0,
	xml_attribute_name/0,
	xml_attribute_value/0,
	xml_attribute/0,
	xml_ns_prefix/0,
	xml_ns_import/0,
	xml_element/0
]).
