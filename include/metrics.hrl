-ifndef( owl_include_metrics_hrl ).
-define( owl_include_metrics_hrl, true ).

-define( duration( ProcedureName, Expr ), owl_log:report_duration( ProcedureName, fun() -> Expr end ) ).

-endif. % owl_include_metrics_hrl
