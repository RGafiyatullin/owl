-module (owl_jid_test).
-include_lib("eunit/include/eunit.hrl").

j2b_test() ->
	J1 = owl_jid:new( <<"node">>, <<"host">>, <<"resource">> ),
	?assertEqual( <<"node@host/resource">>, owl_jid:j2b( J1 ) ),
	J2 = owl_jid:new( <<"room">>, <<"muc.host">>, <<"foreiGner@faRlands/laptop">> ),
	?assertEqual( <<"room@muc.host/foreiGner@faRlands/laptop">>, owl_jid:j2b( J2 ) ).

b2j_test() ->
	B1 = <<"node@host/resource">>,
	B2 = <<"room@muc.host/foreiGner@faRlands/laptop">>,
	J1 = owl_jid:b2j( B1 ),
	J2 = owl_jid:b2j( B2 ),
	?assertEqual( <<"node">>, owl_jid:node( J1 ) ),
	?assertEqual( <<"host">>, owl_jid:host( J1 ) ),
	?assertEqual( <<"resource">>, owl_jid:resource( J1 ) ),
	?assertEqual( <<"room">>, owl_jid:node( J2 ) ),
	?assertEqual( <<"muc.host">>, owl_jid:host( J2 ) ),
	?assertEqual( <<"foreiGner@faRlands/laptop">>, owl_jid:resource( J2 ) ).

host_prep_test() ->
	B1 = <<"foreiGner@faRlands/lapTop">>,
	J1 = owl_jid:b2j( B1 ),
	?assertEqual( <<"foreiGner">>, owl_jid:node( J1 ) ),
	?assertEqual( <<"lapTop">>, owl_jid:resource( J1 ) ),
	?assertEqual( <<"farlands">>, owl_jid:host( J1 ) ).




