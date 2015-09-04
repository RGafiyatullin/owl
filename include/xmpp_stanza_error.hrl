-ifndef(owl_xmpp_include_xmpp_stanza_error_hrl).
-define(owl_xmpp_include_xmpp_stanza_error_hrl, true).

-include_lib("owl_xmpp/include/xmpp_error_common.hrl").

-type xmpp_stanza_error_condition() ::
		   'bad-request'
		|  'conflict'
		|  'feature-not-implemented'
		|  'forbidden'
		|  'gone'
		|  'internal-server-error'
		|  'item-not-found'
		|  'jid-malformed'
		|  'not-acceptable'
		|  'not-allowed'
		|  'not-authorized'
		|  'policy-violation'
		|  'recipient-unavailable'
		|  'redirect'
		|  'registration-required'
		|  'remote-server-not-found'
		|  'remote-server-timeout'
		|  'resource-constraint'
		|  'service-unavailable'
		|  'subscription-required'
		|  'undefined-condition'
		|  'unexpected-request'.

-define( is_xmpp_stanza_error_condition( C ), (
		C == 'bad-request' orelse
		C == 'conflict' orelse
		C == 'feature-not-implemented' orelse
		C == 'forbidden' orelse
		C == 'gone' orelse
		C == 'internal-server-error' orelse
		C == 'item-not-found' orelse
		C == 'jid-malformed' orelse
		C == 'not-acceptable' orelse
		C == 'not-allowed' orelse
		C == 'not-authorized' orelse
		C == 'policy-violation' orelse
		C == 'recipient-unavailable' orelse
		C == 'redirect' orelse
		C == 'registration-required' orelse
		C == 'remote-server-not-found' orelse
		C == 'remote-server-timeout' orelse
		C == 'resource-constraint' orelse
		C == 'service-unavailable' orelse
		C == 'subscription-required' orelse
		C == 'undefined-condition' orelse
		C == 'unexpected-request'
	) ).

-type xmpp_stanza_error_type() ::
	  cancel
	| continue
	| modify
	| auth
	| wait
	| ''.
-define( is_xmpp_stanza_error_type( T ), (
	T == '' orelse
	T == cancel orelse
	T == continue orelse
	T == modify orelse
	T == auth orelse
	T == wait ) ).

-type xmpp_stanza_error_code() :: pos_integer() | undefined.

-record (xmpp_stanza_error_pub, {
		condition = 'internal-server-error' :: xmpp_stanza_error_condition(),
		code = 500 :: xmpp_stanza_error_code(),
		type = 'wait' :: xmpp_stanza_error_type(),
		text = undefined :: binary() | undefined,
		xml_children = [] :: [ term() ],
		condition_xml_children = [] :: [ term() ],
		stanza_xml_children = [] :: [ term() ]
	}).

-record (xmpp_stanza_error, {
		public = #xmpp_stanza_error_pub{} :: #xmpp_stanza_error_pub{},
		common = #xmpp_error_common{} :: #xmpp_error_common{}
	}).

-define( stanza_error( Props ), owl_xmpp_stanza_error:new( [ {'common.origin', {?MODULE, ?LINE}} | Props] ) ).
-define( raise_stanza_error( Props ), owl_xmpp_stanza_error:raise( ?stream_error( Props ) ) ).
-define( catch_match_stanza_error( FunHandle ), error:( CaughtStanzaError = #xmpp_stanza_error{} ) -> FunHandle( CaughtStanzaError ) ).


-define( _ensure( Expr ), case (Expr) of true -> ok; _ -> error( {badarg, ??Expr} ) end ).
-define( _alter_field( FirstField, AsRecord, SecondField, Value ),
			(Err #xmpp_stanza_error{
				FirstField = (
					( Err #xmpp_stanza_error.FirstField )
						#AsRecord{ SecondField = Value }
					)
				}
			)
		).


-endif. % owl_xmpp_include_xmpp_stanza_error_hrl
