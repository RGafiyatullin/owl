-ifndef(owl_xmpp_include_log_hrl).
-define(owl_xmpp_include_log_hrl, true).

-type log_level() :: trace | debug | info | warning | error.
-define( log_levels, [ trace, debug, info, warning, error ] ).

-define(log( Level, Report ), owl_xmpp_log:report( Level, Report ) ).
-define(log_io( Party, Data ), owl_xmpp_log:report_io( Party, Data )).

-endif. % owl_xmpp_include_log_hrl
