-ifndef( owl_xmpp_include_xmpp_stream_error_hrl ).
-define( owl_xmpp_include_xmpp_stream_error_hrl, true ).

-include_lib("owl_xmpp/include/xmpp_error_common.hrl").

-type xmpp_stream_error_condition() ::
	  'bad-format'
	| 'bad-namespace-prefix'
	| 'conflict'
	| 'connection-timeout'
	| 'host-gone'
	| 'host-unknown'
	| 'improper-addressing'
	| 'internal-server-error'
	| 'invalid-form'
	| 'invalid-id'
	| 'invalid-namespace'
	| 'invalid-xml'
	| 'not-authorized'
	| 'policy-violation'
	| 'remote-connection-failed'
	| 'resource-constraint'
	| 'restricted-xml'
	| 'see-other-host'
	| 'system-shutdown'
	| 'undefined-condition'
	| 'unsupported-encoding'
	| 'unsupported-stanza-type'
	| 'unsupported-version'
	| 'not-well-formed'.

-define( is_xmpp_stream_error_condition( C ), in(C, [
	  'bad-format'
	, 'bad-namespace-prefix'
	, 'conflict'
	, 'connection-timeout'
	, 'host-gone'
	, 'host-unknown'
	, 'improper-addressing'
	, 'internal-server-error'
	, 'invalid-form'
	, 'invalid-id'
	, 'invalid-namespace'
	, 'invalid-xml'
	, 'not-authorized'
	, 'policy-violation'
	, 'remote-connection-failed'
	, 'resource-constraint'
	, 'restricted-xml'
	, 'see-other-host'
	, 'system-shutdown'
	, 'undefined-condition'
	, 'unsupported-encoding'
	, 'unsupported-stanza-type'
	, 'unsupported-version'
	, 'not-well-formed'
	]) ).

-record (xmpp_stream_error_pub, {
		condition = 'internal-server-error' :: xmpp_stream_error_condition(),
		text = undefined :: undefined | binary(),
		xml_children = [] :: [ term() ],
		condition_xml_children = [] :: [ term() ]
	}).
-record (xmpp_stream_error, {
		public = #xmpp_stream_error_pub{} :: #xmpp_stream_error_pub{},
		common = #xmpp_error_common{} :: #xmpp_error_common{}
	}).

-define( stream_error( Props ), owl_stream_error:new( [ {'common.origin', {?MODULE, ?LINE}} | Props] ) ).
-define( raise_stream_error( Props ), owl_stream_error:raise( ?stream_error( Props ) ) ).
-define( catch_match_stream_error( FunHandle ), error:( CaughtStreamError = #xmpp_stream_error{} ) -> FunHandle( CaughtStreamError ) ).


-define( _ensure( Expr ), case (Expr) of true -> ok; _ -> error( {badarg, ??Expr} ) end ).
-define( _alter_field( FirstField, AsRecord, SecondField, Value ),
			(Err #xmpp_stream_error{
				FirstField = (
					( Err #xmpp_stream_error.FirstField )
						#AsRecord{ SecondField = Value }
					)
				}
			)
		).


-endif. % owl_xmpp_include_xmpp_stream_error_hrl
