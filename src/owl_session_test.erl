-module (owl_session_test).

-include_lib("eunit/include/eunit.hrl").
-include("ns_xmpp_core.hrl").
-include("const.hrl").

-define( my_assertMatch( Guard, Expr ),
	begin
		( fun() ->
			__V = (Expr),
			case __V of
				Guard -> __V;
				_ -> ?assertMatch( Guard, __V )
			end
		end ) ()
	end ).

t1_test() ->
	{ok, StreamSrv} = ?my_assertMatch({ok, _}, owl_stream_tcp:start_link( tcp_connect, "localhost", 5222, [] ) ),
	{ok, SessionSrv} = ?my_assertMatch({ok, _}, owl_session:start_link( StreamSrv, [] ) ),
	{ok, _AuthService} = ?my_assertMatch({ok, _}, owl_session:add_service( SessionSrv,
														auth, owl_session_service_sasl_auth,
														[
															{auth_cid, <<"admin">>},
															{auth_host, <<"admin.localhost">>},
															{auth_secret, <<"admin-pw">>}
														] )),
	{ok, _BindService} = ?my_assertMatch({ok, _}, owl_session:add_service( SessionSrv,
														bind, owl_session_service_bind,
														[
															{resource_to_bind, <<"a-resource-to-bind">>}
														])),
	{ok, _SessionService} = ?my_assertMatch({ok, _}, owl_session:add_service( SessionSrv,
														session, owl_session_service_session, [])),
	{ok, _PresenceService} = ?my_assertMatch({ok, _}, owl_session:add_service( SessionSrv,
														presence, owl_session_service_presence, [])),
	{ok, _InboundIQService} = ?my_assertMatch({ok, _}, owl_session:add_service( SessionSrv,
														inbound_iq, owl_session_service_inbound_iq, [])),

	?assertMatch( ok, owl_session:launch_services( SessionSrv ) ),

	?assertMatch( ok, owl_stream_tcp:controlling_process( StreamSrv, SessionSrv ) ),
	?assertMatch( ok, owl_stream_tcp:send_stream_open( StreamSrv, [ {<<"to">>, <<"admin.localhost">>} ] ) ),

	ok.

