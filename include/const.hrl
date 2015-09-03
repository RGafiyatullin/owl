-ifndef( owl_xmpp_include_const_hrl ).
-define( owl_xmpp_include_const_hrl, true ).

-define( call_timeout_short, 1000 ).
-define( call_timeout_normal, 5000 ).
-define( call_timeout_long, 30000 ).

-define( prio_first, (-50) ).
-define( prio_early, (-25) ).
-define( prio_normal, 0 ).
-define( prio_late, 25 ).
-define( prio_last, 50 ).

-define( prio_before( P ), ( P - 5 ) ).
-define( prio_after( P ), ( P + 5 ) ).


-endif. % owl_xmpp_include_const_hrl
