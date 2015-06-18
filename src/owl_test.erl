-module (owl_test).
-export ([prepare/0]).
-include_lib("eunit/include/eunit.hrl").

prepare() ->
	application:start(sasl).

t1_test() ->
	{ok, Owl} = owl_tcp:start_link( tcp_connect, "localhost", 5222, [] ),
	ok = owl_tcp:send_stream_open( Owl, [ {<<"to">>, <<"admin.localhost">>} ] ),
	ok = owl_tcp:set_active( Owl, true ),
	{ok, Owl}.
