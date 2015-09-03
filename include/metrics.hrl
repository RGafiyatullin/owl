-ifndef( owl_xmpp_include_metrics_hrl ).
-define( owl_xmpp_include_metrics_hrl, true ).

-define( duration( ProcedureName, Expr ), owl_xmpp_log:report_duration( ProcedureName, fun() -> Expr end ) ).

-endif. % owl_xmpp_include_metrics_hrl
