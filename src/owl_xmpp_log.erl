-module (owl_xmpp_log).
-compile ({parse_transform, gin}).
-export ([
		report/2,
		report_io/2,
		report_duration/2
	]).

report( Info, Report ) when in(Info, [trace, debug, info]) -> error_logger:info_report( [self() | Report] );
report( warning, Report ) -> error_logger:warning_report( [self() | Report] );
report( error, Report ) -> error_logger:error_report( [self() | Report] ).

report_io( _Party, _IO ) ->
	ok.
	% error_logger:info_report( [ io, {party, Party}, {io, IO} ] ).



report_duration( ProcedureName, F ) ->
	{Duration, RetValue} = timer:tc( F ),
	ok = report( trace, [ {procedure, ProcedureName}, {duration, Duration} ] ),
	RetValue.

