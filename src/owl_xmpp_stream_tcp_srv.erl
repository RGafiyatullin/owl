-module (owl_xmpp_stream_tcp_srv).
-compile ({parse_transform, gin}).
-behaviour (gen_server).

-export ([
		start_link/3,
		start_link/4
	]).

-export ([
		init/1,
		enter_loop_with_socket/5, enter_loop_with_tcp_endpoint/5,

		handle_call/3,
		handle_cast/2,
		handle_info/2,
		code_change/3,
		terminate/2
	]).

-include_lib ("expellee/include/xml.hrl").
-include_lib ("expellee/include/sax.hrl").

-include ("owl_stream_tcp_internals.hrl").

-include ("log.hrl").
-include ("ns_xmpp_core.hrl").
-include ("xmpp_stream_events.hrl").

-define ( start_link_timeout, 5000 ).
-define ( hibernate_timeout, infinity ).

-define ( normal_shutdown_reason( S ), normal ).
-define ( hib_timeout( S ), (S #s.hibernate_timeout) ).


-spec start_link(
		ControllingProcess :: pid(),
		InitFrom :: xmpp_tcp_init_arg(),
		XmppTcpOpts :: [ xmpp_tcp_option() ]
	) ->
		{ok, pid()}.
-spec start_link(
		ControllingProcess :: pid(),
		InitFrom :: xmpp_tcp_init_arg(),
		XmppTcpOpts :: [ xmpp_tcp_option() ],
		StartLinkTimeout :: timeout()
	) ->
		{ok, pid()}.

-type gen_reply_to() :: {pid(), reference()}.


start_link( ControllingProcess, InitFrom, XmppTcpOpts ) ->
	start_link( ControllingProcess, InitFrom, XmppTcpOpts, ?start_link_timeout ).

start_link( ControllingProcess, InitFrom, XmppTcpOpts, StartLinkTimeout ) ->
	StartLinkFun = proplists:get_value( start_link_fun, XmppTcpOpts, fun proc_lib:start_link/4 ),
	case InitFrom of
		{socket, RanchTransport, RanchSocket} ->
			OwnershipGrantedMessage = {?MODULE, ownership_granted, RanchTransport, RanchSocket},
			case StartLinkFun(
				?MODULE, enter_loop_with_socket,
				[ ControllingProcess, XmppTcpOpts, RanchTransport,
				  RanchSocket, OwnershipGrantedMessage ],
				StartLinkTimeout
			) of
				{ok, Pid} when is_pid( Pid ) ->
					ok = RanchTransport:controlling_process( RanchSocket, Pid ),
					Pid ! OwnershipGrantedMessage,
					{ok, Pid};
				Error ->
					Error
			end;

		{tcp_endpoint, Host, Port, TcpOpts} ->
			StartLinkFun(
				?MODULE, enter_loop_with_tcp_endpoint,
				[ ControllingProcess, XmppTcpOpts, Host, Port, TcpOpts ],
				StartLinkTimeout )
	end.

-record(s, {
		'#module' = ?MODULE,

		opts :: [ tuple() ],

		controlling_process :: pid(),
		controlling_process_monref :: reference(),
		heir_process :: pid(),

		hibernate_timeout :: infinity | non_neg_integer(),

		xml_stream_fqn :: undefined | { binary(), binary() },

		xml_stream_render_ctx :: undefined | exp_render_stream:ctx(),
		xml_stanza_render_ctx :: undefined | exp_render_stream:ctx(),

		downstream_iodata_buffer = ?queue:new() :: queue_t( iodata() ),

		ranch_transport :: atom(),
		ranch_socket :: term(),
		ranch_messages :: undefined | { TagOk :: atom(), TagClosed :: atom(), TagError :: atom() },

		is_active = false :: true | false | once,
		socket_activated = false,

		xml_stream_parse_sax :: undefined | exp_parse_sax:sax(),
		xml_stream_parse_pending_sax_events = ?queue:new() :: queue_t( sax_event() ),
		xml_stream_parse_sax2dom = undefined :: undefined | exp_sax2dom:ctx(),


		pending_passive_receivers = ?queue:new() :: queue_t( gen_reply_to() ),
		xmpp_stream_event_queue = ?queue:new() :: queue_t( owl_xmpp:xmpp_stream_event() ),

		shutdown = false :: boolean(),
		shutdown_reason = undefined :: undefined | term()
	}).

-type state() :: #s{}.


enter_loop_with_socket( ControllingProcess, XmppTcpOpts, RanchTransport, RanchSocket, OwnershipGrantedMessage ) ->
	InitAckFun = proplists:get_value( init_ack_fun, XmppTcpOpts, fun proc_lib:init_ack/1 ),
	InitAckFun({ok, self()}),

	HibTimeout = proplists:get_value( hibernate_timeout, XmppTcpOpts, ?hibernate_timeout ),
	S0 = #s{
			opts = XmppTcpOpts,

			controlling_process = ControllingProcess,
			hibernate_timeout = HibTimeout,

			ranch_transport = RanchTransport,
			ranch_socket = RanchSocket
		},
	{ok, S1} = do_maybe_wait_for_ownership_granted_messages( OwnershipGrantedMessage, S0 ),

	S2 = S1 #s{ xml_stream_fqn = opts_get_stream_fqn(XmppTcpOpts) },
	{ok, S3} = do_initialize_xml_render_context( XmppTcpOpts, S2 ),
	{ok, S4} = do_initialize_xml_parser_context( XmppTcpOpts, S3 ),
	{ok, S5} = do_initialize_transport( XmppTcpOpts, S4 ),
	{ok, S6} = do_install_monitor_for_controlling_process( S5 ),

	SInitialized = S6,
	gen_server:enter_loop( ?MODULE, [], SInitialized, ?hib_timeout( SInitialized ) ).

enter_loop_with_tcp_endpoint( ControllingProcess, XmppTcpOpts, TcpEndpointHost, TcpEndpointPort, TcpOpts ) ->
	case ranch_tcp:connect( TcpEndpointHost, TcpEndpointPort, TcpOpts ) of
		{ok, RanchSocket} ->
			enter_loop_with_socket( ControllingProcess, XmppTcpOpts, ranch_tcp, RanchSocket, undefined );
		{error, _Reason} = Error ->
			Error
	end.


init( {} ) -> {stop, enter_loop_used}.

handle_call( ?controlling_process( Old, New ), ReplyTo, S ) ->
	handle_call_controlling_process( Old, New, ReplyTo, S );

handle_call( ?set_active( Mode ), ReplyTo, S = #s{} ) ->
	handle_call_set_active( Mode, ReplyTo, S );

handle_call( ?send_stream_open( StreamAttrs, ReinitParserCtx ), ReplyTo, S = #s{} ) ->
	handle_call_send_stream_open( StreamAttrs, ReinitParserCtx, ReplyTo, S );

handle_call( ?send_stanza( Stanza ), ReplyTo, S ) ->
	handle_call_send_stanza( Stanza, ReplyTo, S );

handle_call( ?send_stream_close(), ReplyTo, S ) ->
	handle_call_send_stream_close( ReplyTo, S );

handle_call( Unexpected, ReplyTo, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_call, {unexpected_request, Unexpected}, {reply_to, ReplyTo} ]),
	{reply, badarg, S, ?hib_timeout( S )}.

handle_cast( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_cast, {unexpected_request, Unexpected} ]),
	{noreply, S, ?hib_timeout( S )}.

handle_info( timeout, S ) ->
	?log(debug, [ ?MODULE, handle_info_timeout, hibernating ]),
	{noreply, S, hibernate};

handle_info( {'DOWN', MonRef, process, ControllingProcess, Reason}, State = #s{ controlling_process = ControllingProcess, controlling_process_monref = MonRef } ) ->
	handle_info_controlling_process_down( Reason, State );

handle_info( {TagOk, Socket, Data}, S = #s{ ranch_messages = ?match_tag_ok( TagOk ), ranch_socket = Socket } ) ->
	handle_info_socket_data( Data, S );

handle_info( {TagClosed, Socket}, S = #s{ ranch_messages = ?match_tag_closed( TagClosed ), ranch_socket = Socket } ) ->
	handle_info_socket_closed( S );

handle_info( {TagError, Socket, SocketError}, S = #s{ ranch_messages = ?match_tag_error( TagError ), ranch_socket = Socket } ) ->
	handle_info_socket_error( SocketError, S );

handle_info( Unexpected, S = #s{} ) ->
	?log(warning, [ ?MODULE, handle_info, {unexpected_request, Unexpected} ]),
	{noreply, S, ?hib_timeout( S )}.

code_change( OldVsn, Info, S ) ->
	?log(info, [?MODULE, code_change, {old_vsn, OldVsn}, {info, Info}]),
	{ok, S}.

terminate( _Reason, _S ) ->
	ignore.


handle_info_controlling_process_down( Reason, S0 = #s{} ) ->
	?log( debug, [?MODULE, handle_info_controlling_process_down, {reason, Reason} ] ),
	{ok, S1} = do_reply_to_all_passive_receivers( {error, closed}, S0 ),
	{stop, ?normal_shutdown_reason( S1 ), S1}.


handle_info_socket_data( Data, S0 ) ->
	?log_io( peer, Data ),
	S1 = S0 #s{ socket_activated = false },

	{ok, S2} = do_call_sax_parse( Data, S1 ),
	{ok, S3} = do_process_sax_events( S2 ),

	{noreply, S3, ?hib_timeout(S3)}.

handle_info_socket_error( SocketError, S0) ->
	?log(info, [?MODULE, handle_info_socket_error, {socket_error, SocketError}]),
	S1 = S0 #s{ socket_activated = false },
	do_reply_to_all_passive_receivers_and_maybe_shutdown(SocketError, S1).


handle_info_socket_closed( S0 = #s{} ) ->
	?log(info, [?MODULE, handle_info_socket_closed]),
	S1 = S0 #s{ socket_activated = false },
	do_reply_to_all_passive_receivers_and_maybe_shutdown(closed, S1).


do_reply_to_all_passive_receivers_and_maybe_shutdown( ReplyReason, S0 = #s{ controlling_process = ControllingProcess } ) ->
	{ok, S1} = do_reply_to_all_passive_receivers({error, ReplyReason}, S0),
	case is_active(S1) of
		false ->
			{ok, S2} = do_mark_shutdown( ReplyReason, S1 ),
			{noreply,
				S2,
				?hib_timeout(S2)};
		TrueOrOnce when TrueOrOnce == true orelse TrueOrOnce == once ->
			SocketClosedMessage =
				case ReplyReason of
					closed -> ?xmpp_closed(self());
					Other -> ?xmpp_socket_error(self(), Other)
				end,
			ControllingProcess ! SocketClosedMessage,
			{stop, ?normal_shutdown_reason(S1), S1}
	end.


handle_call_set_active( IsActiveNew, ReplyTo, S0 = #s{ is_active = IsActiveOld } ) ->
	case IsActiveOld == IsActiveNew of
		true -> {reply, ok, S0, ?hib_timeout(S0)};
		false ->
			_ = gen_server:reply( ReplyTo, ok ),
			S1 = S0 #s{ is_active = IsActiveNew },
			{ok, S2} = do_dispatch_xmpp_stream_events( S1 ),
			{noreply, S2, ?hib_timeout(S2)}
	end.

handle_call_controlling_process(
	Old, New, _ReplyTo,
	S0 = #s{
		controlling_process = CurrentControllingProcess
	}
) ->
	case Old == CurrentControllingProcess of
		false ->
			{reply, {error, not_owner}, S0};
		true ->
			{ok, S1} = do_uninstall_monitor_for_controlling_process( S0 ),
			{ok, S2} = do_install_monitor_for_controlling_process( S1 #s{ controlling_process = New } ),

			_ = erlang:send( New, ?xmpp_control_handed_over( self() ) ),

			{reply, ok, S2}
	end.

handle_call_send_stream_open(
		_, _, _, S = #s{ shutdown = true, shutdown_reason = Reason }
	) ->
		{reply, {error, Reason}, S, ?hib_timeout(S)};

handle_call_send_stream_open(
	StreamAttrs, ReinitParserCtx, _ReplyTo,
	S0 = #s{
		opts = XmppTcpOpts,

		xml_stream_fqn = {StreamNS, StreamNCN},
		xml_stream_render_ctx = StreamRenderCtx
	}
) ->
	?log( debug, [?MODULE, handle_call_send_stream_open, {stream_attrs, StreamAttrs}] ),
	{OpenTagIOL, StanzaRenderCtx} = exp_render_stream:node_open_tag( StreamNS, StreamNCN, StreamAttrs, StreamRenderCtx ),
	S1 = S0 #s{ xml_stanza_render_ctx = StanzaRenderCtx },
	{ok, S2} = do_socket_write( [ <<"<?xml version='1.0'?>">>, OpenTagIOL], S1 ),
	{ok, S3} = do_socket_flush( S2 ),
	{ok, S5} =
		case ReinitParserCtx of
			false -> {ok, S3};
			true ->
				{ok, S4} = do_destroy_xml_parser_context( S3 ),
				{ok, _S5} = do_initialize_xml_parser_context( XmppTcpOpts, S4 )
		end,
	{reply, ok, S5, ?hib_timeout(S5)}.


handle_call_send_stanza(
		_, _, S = #s{ shutdown = true, shutdown_reason = Reason }
	) ->
		{reply, {error, Reason}, S, ?hib_timeout(S)};

handle_call_send_stanza(
	Stanza, _ReplyTo,
	S0 = #s{
		xml_stanza_render_ctx = StanzaRenderCtx
	}
) ->
	?log( debug, [ ?MODULE, handle_call_send_stanza, {stanza, Stanza} ]),
	case StanzaRenderCtx of
		undefined ->
			{reply, {error, no_stream}, S0, ?hib_timeout(S0)};
		StanzaRenderCtx0 ->
			{ StanzaIOL, StanzaRenderCtx1 } = exp_render:render( Stanza, StanzaRenderCtx0 ),
			S1 = S0 #s{ xml_stanza_render_ctx = StanzaRenderCtx1 },
			{ok, S2} = do_socket_write( StanzaIOL, S1 ),
			{ok, S3} = do_socket_flush( S2 ),
			{reply, ok, S3, ?hib_timeout(S3)}
	end.


handle_call_send_stream_close(
		_, S = #s{ shutdown = true, shutdown_reason = Reason }
	) ->
		{reply, {error, Reason}, S, ?hib_timeout(S)};

handle_call_send_stream_close( ReplyTo, S0 = #s{ xml_stanza_render_ctx = StanzaRenderCtx0, xml_stream_fqn = {StreamNS, StreamNCN} } ) ->
	?log( debug, [ ?MODULE, handle_call_send_stream_close ] ),
	case StanzaRenderCtx0 of
		undefined ->
			{reply, {error, no_stream}, S0, ?hib_timeout(S0)};
		_Defined ->
			{ CloseTagIOL, _ } = exp_render_stream:node_close_tag( StreamNS, StreamNCN, StanzaRenderCtx0 ),
			{ok, S1} = do_socket_write( CloseTagIOL, S0 #s{ xml_stanza_render_ctx = undefined }),
			{ok, S2} = do_socket_flush( S1 ),
			_ = gen_server:reply( ReplyTo, ok ),

			{ok, S3} = do_reply_to_all_passive_receivers( {error, closed}, S2 ),
			{ok, S4} = do_mark_shutdown( closed, S3 ),
			{noreply, S4, ?hib_timeout( S4 )}
	end.





do_dispatch_xmpp_stream_events(
	S0 = #s{
		shutdown = ShouldShutdown,
		shutdown_reason = ShutdownReason,
		controlling_process = ControllingProcess
	}
) ->
	IsActive = is_active( S0 ),
	HasSaxEvents = has_sax_events( S0 ),
	HasPassiveReciever = has_passive_receiver( S0 ),
	HasXmppStreamEvent = has_xmpp_stream_event( S0 ),
	?log( debug, [?MODULE, do_dispatch_xmpp_stream_events,
		{is_active, IsActive}, {has_sax_events, HasSaxEvents},
		{has_passive_receiver, HasPassiveReciever}, {has_xmpp_stream_event, HasXmppStreamEvent}]),
	case {IsActive, HasSaxEvents, HasPassiveReciever, HasXmppStreamEvent} of
		{_, _, true, true} ->
			{ok, S1} = do_dispatch_event_to_passive_receiver( S0 ),
			do_dispatch_xmpp_stream_events( S1 );

		{_, true, true, false} ->
			do_process_sax_events( S0 );

		{_, false, true, false} ->
			{ok, _S1} = do_socket_activate( S0 );



		{true, _, false, true} ->
			{ok, S1} = do_dispatch_event_to_controlling_process( S0 ),
			do_dispatch_xmpp_stream_events( S1 );

		{true, true, false, false} ->
			do_process_sax_events( S0 );

		{true, false, false, false} when not ShouldShutdown ->
			{ok, _S1} = do_socket_activate( S0 );

		{true, false, false, false} when ShouldShutdown ->
			SocketClosedMessage =
				case ShutdownReason of
					closed -> ?xmpp_closed(self());
					Other -> ?xmpp_socket_error(self(), Other)
				end,
			ControllingProcess ! SocketClosedMessage,
			erlang:exit(?normal_shutdown_reason(S0));

		{false, _, false, _} ->
			{ok, S0}
	end.

is_active( #s{ is_active = IsActive } ) -> in( IsActive, [ true, once ] ).
has_passive_receiver( #s{ pending_passive_receivers = PPR_Q } ) -> not ?queue:is_empty( PPR_Q ).
has_xmpp_stream_event( #s{ xmpp_stream_event_queue = XSE_Q } ) -> not ?queue:is_empty( XSE_Q ).
has_sax_events( #s{ xml_stream_parse_pending_sax_events = SEQ } ) -> not ?queue:is_empty( SEQ ).

do_dispatch_event_to_controlling_process( S0 = #s{ controlling_process = ControllingProcess, is_active = IsActive0 } ) ->
	{ok, Event, S1} = do_fetch_event( S0 ),
	_ = erlang:send( ControllingProcess, Event ),
	S2 =
		case IsActive0 of
			once -> S1 #s{ is_active = false };
			_ -> S1
		end,
	{ok, S2}.

do_dispatch_event_to_passive_receiver( S0 = #s{} ) ->
	{ok, Event, S1} = do_fetch_event( S0 ),
	{ok, Receiver_ReplyTo, S2} = do_fetch_receiver( S1 ),
	_ = gen_server:reply( Receiver_ReplyTo, Event ),
	{ok, S2}.

do_fetch_event( S0 = #s{ xmpp_stream_event_queue = XSE_Q0 } ) ->
	{value, Event} = ?queue:peek( XSE_Q0 ),
	XSE_Q1 = ?queue:drop( XSE_Q0 ),
	S1 = S0 #s{ xmpp_stream_event_queue = XSE_Q1 },
	{ok, Event, S1}.

do_fetch_receiver( S0 = #s{ pending_passive_receivers = PPR_Q0 } ) ->
	{value, Receiver_ReplyTo} = ?queue:peek( PPR_Q0 ),
	PPR_Q1 = ?queue:drop( PPR_Q0 ),
	S1 = S0 #s{ pending_passive_receivers = PPR_Q1 },
	{ok, Receiver_ReplyTo, S1}.


do_process_sax_events( S0 = #s{ xml_stream_parse_pending_sax_events = SEQ0 } ) ->
	case ?queue:peek( SEQ0 ) of
		{value, SaxEvent} ->
			SEQ1 = ?queue:drop( SEQ0 ),
			S1 = S0 #s{ xml_stream_parse_pending_sax_events = SEQ1 },
			do_process_sax_event( SaxEvent, S1 );

		empty ->
			do_dispatch_xmpp_stream_events( S0 )
	end.


do_process_sax_event(
		#xml_element_start{ ns = StreamNS, ncn = StreamNCN, attrs = StreamAttrs },
		S0 = #s{ xml_stream_fqn = {StreamNS, StreamNCN}, xml_stream_parse_sax2dom = undefined }
	) ->
		Event = ?xmpp_stream_open( self(), StreamAttrs ),
		{ok, S1} = do_enqueue_stream_event( Event, S0 ),
		do_dispatch_xmpp_stream_events( S1 );

do_process_sax_event(
		#xml_element_end{ ns = StreamNS, ncn = StreamNCN },
		S0 = #s{ xml_stream_fqn = {StreamNS, StreamNCN}, xml_stream_parse_sax2dom = undefined }
	) ->
		Event = ?xmpp_stream_close( self() ),
		{ok, S1} = do_enqueue_stream_event( Event, S0 ),
		{ok, S2} = do_dispatch_event_to_controlling_process( S1 ),
		{ok, S3} = do_reply_to_all_passive_receivers( {error, closed}, S2 ),
		{ok, _S4} = do_mark_shutdown( closed, S3 );

do_process_sax_event(
		StanzaSaxEvent,
		S0 = #s{ xml_stream_parse_sax2dom = Sax2Dom0 }
	) ->
		Sax2Dom1 =
			case Sax2Dom0 of
				undefined ->
					{ok, S2D} = exp_sax2dom:new(),
					S2D;
				S2D ->
					S2D
			end,
		case exp_sax2dom:sax_event_in( StanzaSaxEvent, Sax2Dom1 ) of
			{final, Stanza} ->
				Event = ?xmpp_stanza( self(), Stanza ),
				{ok, S1} = do_enqueue_stream_event( Event, S0 #s{ xml_stream_parse_sax2dom = undefined } ),
				do_dispatch_xmpp_stream_events( S1 );
			{ok, Sax2Dom2} ->
				S1 = S0 #s{ xml_stream_parse_sax2dom = Sax2Dom2 },
				do_process_sax_events( S1 );
			{error, S2DError} ->
				?log(info, [ ?MODULE, do_process_sax_event, {sax2dom_error, S2DError} ]),
				Event = ?xmpp_error( self(), peer, true, {sax2dom_error, S2DError}, [] ),
				{ok, S1} = do_enqueue_stream_event( Event, S0 ),
				{ok, S2} = do_dispatch_event_to_controlling_process( S1 ),
				{ok, S3} = do_reply_to_all_passive_receivers( Event, S2 ),
				{ok, _S4} = do_mark_shutdown( closed, S3 )
		end.

do_call_sax_parse( Data, S0 = #s{ xml_stream_parse_sax = Sax0, xml_stream_parse_pending_sax_events = SEQ0 }) ->
	case exp_parse_sax:parse( Data, Sax0 ) of
		{ok, SaxEvents, Sax1} ->
			SEQ1 = lists:foldl( fun ?queue:in/2, SEQ0, SaxEvents ),
			S1 = S0 #s{ xml_stream_parse_sax = Sax1, xml_stream_parse_pending_sax_events = SEQ1 },
			{ok, S1};
		{error, ParseError, Sax1} ->
			?log(info, [ ?MODULE, do_call_sax_parse, {parse_error, ParseError} ]),

			S1 = S0 #s{ xml_stream_parse_sax = Sax1 }, %% WHY?!?! owl_xmpp_stream_tcp_srv.erl:...: The attempt to match a term of type exp_parse_sax:sax() against the variable Sax1 breaks the opaqueness of the term

			Event = ?xmpp_error( self(), peer, true, {parse_error, ParseError}, [] ),
			{ok, S2} = do_enqueue_stream_event( Event, S1 ),
			{ok, S3} = do_dispatch_event_to_controlling_process( S2 ),
			{ok, S4} = do_reply_to_all_passive_receivers( Event, S3 ),
			{ok, _S5} = do_mark_shutdown( closed, S4 )
	end.

do_enqueue_stream_event( Event, S0 = #s{ xmpp_stream_event_queue = XSE_Q0 } ) ->
	XSE_Q1 = ?queue:in( Event, XSE_Q0 ),
	S1 = S0 #s{ xmpp_stream_event_queue = XSE_Q1 },
	{ok, S1}.


do_reply_to_all_passive_receivers( ReplyWith, S0 = #s{ pending_passive_receivers = PPR_Q } ) ->
	case ?queue:peek( PPR_Q ) of
		empty -> {ok, S0};
		{value, ReplyTo} ->
			_ = gen_server:reply( ReplyTo, ReplyWith ),
			do_reply_to_all_passive_receivers( ReplyWith, S0 #s{ pending_passive_receivers = ?queue:drop( PPR_Q ) } )
	end.


do_mark_shutdown( Reason, S0 ) ->
	{ok, S0 #s{ shutdown = true, shutdown_reason = Reason }}.

do_maybe_wait_for_ownership_granted_messages( OwnershipGrantedMessage, S ) ->
	case OwnershipGrantedMessage of
		undefined -> ok;
		Defined ->
			% No need for timeout here as if the current process overdues - it is killed (start_link's responsibility)
			receive Defined -> ok end
	end,
	{ok, S}.

do_initialize_xml_render_context( XmppTcpOpts, S0 ) ->
	NSImports = opts_get_xml_render_ns_imports( XmppTcpOpts ),
	RenderCtx = exp_render_stream:ctx_new( [ {ns_import, NS, Prefix} || {NS, Prefix} <- NSImports ] ),
	{ok, S0 #s{ xml_stream_render_ctx = RenderCtx }}.

do_destroy_xml_parser_context( S0 = #s{ xml_stream_parse_sax = undefined } ) -> {ok, S0};
do_destroy_xml_parser_context( S0 = #s{ xml_stream_parse_sax = Sax } ) ->
	ok = exp_parse_sax:free( Sax ),
	S1 = S0 #s{ xml_stream_parse_sax = undefined },
	{ok, S1}.


do_initialize_xml_parser_context( _XmppTcpOpts, S0 ) ->
	{ok, Sax} = exp_parse_sax:new(),
	{ok, S0 #s{ xml_stream_parse_sax = Sax }}.

do_initialize_transport( _XmppTcpOpts, S0 = #s{ ranch_transport = TransportMod } ) ->
	RanchMessages = TransportMod:messages(),
	S1 = S0 #s{ ranch_messages = RanchMessages },
	{ok, S1}.

do_uninstall_monitor_for_controlling_process( S0 = #s{ controlling_process_monref = MonRef } ) ->
	_ = erlang:demonitor( MonRef, [flush] ),
	S1 = S0 #s{ controlling_process_monref = undefined },
	{ok, S1}.

do_install_monitor_for_controlling_process( S0 = #s{ controlling_process = ControllingProcess } ) ->
	MonRef = erlang:monitor( process, ControllingProcess ),
	S1 = S0 #s{ controlling_process_monref = MonRef },
	{ok, S1}.





opts_get_stream_fqn( XmppTcpOpts ) ->
	proplists:get_value(
		xml_stream_fqn, XmppTcpOpts,
		{ ?ns_jabber_streams, <<"stream">> } ).

opts_get_xml_render_ns_imports( XmppTcpOpts ) ->
	proplists:get_value(
		xml_render_ns_imports, XmppTcpOpts,
		[ {?ns_jabber_streams, <<"streams">>}, {?ns_jabber_client, <<>>} ] ).



-spec do_socket_write( iodata(), state() ) ->
	{ok, state()}.
-spec do_socket_flush( state() ) ->
	{ok, state()}.
-spec do_transport_send( iodata(), state() ) ->
	{ok, state()} | {error, Reason :: atom()}.


do_socket_write( IOL, S0 = #s{ downstream_iodata_buffer = Buff0 } ) ->
	Buff1 = ?queue:in( IOL, Buff0 ),
	{ok, S0 #s{ downstream_iodata_buffer = Buff1 }}.

do_socket_flush( S0 = #s{ downstream_iodata_buffer = Buff } ) ->
	BuffIOL = ?queue:to_list( Buff ),
	S1 = S0 #s{ downstream_iodata_buffer = ?queue:new() },
	?log_io( self, BuffIOL ),

	{ok, _S2} = do_transport_send( BuffIOL, S1 ).

do_transport_send( IOL, S0 = #s{ ranch_transport = TransportMod, ranch_socket = SocketHandle } ) ->
	case TransportMod:send( SocketHandle, IOL ) of
		ok -> {ok, S0};
		{error, Reason} ->
			?log( info, [?MODULE, do_transport_send, {socket_error_upon_send, Reason}] ),
			{ok, S0}
	end.


do_socket_activate( S0 = #s{ socket_activated = true } ) -> {ok, S0};
do_socket_activate( S0 = #s{ socket_activated = false, ranch_transport = TransportMod, ranch_socket = SocketHandle } ) ->
	ok = TransportMod:setopts( SocketHandle, [ {active, once} ] ),
	{ok, S0 #s{ socket_activated = true }}.


% is_xmpp_error_terminal( ?xmpp_error( _Xmpp, _CausingParty, IsTerminal, _Type, _Details ) ) -> IsTerminal;
% is_xmpp_error_terminal( undefined ) -> false.
