-ifndef( owl_xmpp_include_xmpp_error_common_hrl ).
-define( owl_xmpp_include_xmpp_error_common_hrl, true ).

-record (xmpp_error_common, {
		origin :: {string(), non_neg_integer()} | undefined,
		stack :: undefined | [ term() ],
		reason :: undefined | term(),
		unknown_props = [] :: [term()]
	}).

-endif. % owl_xmpp_include_xmpp_error_common_hrl
