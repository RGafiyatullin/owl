-ifndef(owl_include_owl_stream_tcp_internals_hrl).
-define(owl_include_owl_stream_tcp_internals_hrl, true).

-type queue_t( T ) :: queue:queue( T ).
-define( queue, queue ).


-type start_link_fun() :: fun(
		( atom(), atom(), [term()], timeout() )
			-> {ok, pid()} | {error, term()}
	).
-type init_ack_fun() :: fun(
		( term() ) -> ok
	).

-type xmpp_tcp_option() ::
	    {start_link_fun, start_link_fun()}
	  | {init_ack_fun, init_ack_fun()}
	  | {hibernate_timeout, timeout()}
	  | {xml_render_ns_imports, [ {binary(), binary()} ]}
	  | {xml_stream_fqn, {binary(), binary()}}.

-type gen_tcp_connect_option() :: gen_tcp:connect_option().
-type xmpp_tcp_init_arg() ::
	  {socket, atom(), term()}
	| {tcp_endpoint,
		inet:ip_address() | inet:hostname(),
		inet:port_number(),
		[ gen_tcp_connect_option() ]}.

-define( match_tag_closed( TagClosed ), {_, TagClosed, _} ).
-define( match_tag_ok( TagOk ), {TagOk, _, _} ).
-define( match_tag_error( TagError ), {_, _, TagError} ).

-define( controlling_process( Old, New ), {controlling_process, Old, New}).
-define( send_stream_open( StreamAttrs ), { send_stream_open, StreamAttrs } ).
-define( send_stanza( Stanza ), { send_stanza, Stanza } ).
-define( set_active( Mode ), { set_active, Mode } ).
-define( send_stream_close(), send_stream_close ).

-endif. % owl_include_owl_stream_tcp_internals_hrl
