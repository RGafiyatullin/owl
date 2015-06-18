-module (owl_log).
-compile ({parse_transform, gin}).
-export ([
		report/2,
		report_io/2
	]).

report( Info, Report ) when in(Info, [trace, debug, info]) -> error_logger:info_report( Report );
report( warning, Report ) -> error_logger:warning_report( Report );
report( error, Report ) -> error_logger:error_report( Report ).

report_io( Party, IO ) ->
	error_logger:info_report( [ io, {party, Party}, {io, IO} ] ).



