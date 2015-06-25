-ifndef( owl_include_xmpp_error_common_hrl ).
-define( owl_include_xmpp_error_common_hrl, true ).

-record (xmpp_error_common, {
		origin :: {string(), non_neg_integer()} | undefined,
		stack :: undefined | [ term() ],
		reason :: undefined | term(),
		unknown_props = [] :: [term()]
	}).

-endif. % owl_include_xmpp_error_common_hrl
