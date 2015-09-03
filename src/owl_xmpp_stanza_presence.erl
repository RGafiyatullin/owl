-module (owl_xmpp_stanza_presence).
-compile ({parse_transform, gin}).
-export ([
		is_presence/1, type/1,

		stanza_new/1, stanza_new/2, stanza_new/3,

		type2bin/1, bin2type/1
	]).

-include("ns_xmpp_core.hrl").

-define(all_presence_types, [available, unavailable, subscribe, unsubscribe, subscribed, unsubscribed, probe, error]).
-define(all_presence_types_bin, [<<"available">>, <<"unavailable">>, <<"subscribe">>, <<"unsubscribe">>, <<"subscribed">>, <<"unsubscribed">>, <<"probe">>, <<"error">>]).

is_presence( Xml ) ->
	{?ns_jabber_client, <<"presence">>} == exp_node:fqn( Xml ).

type( Presence ) ->
	true = is_presence( Presence ),
	TypeBin =
		case exp_node_attrs:attr( <<"type">>, Presence ) of
			undefined -> <<"available">>;
			Defined -> Defined
		end,
	bin2type( TypeBin ).

stanza_new( Type ) ->
	stanza_new( Type, [] ).

stanza_new( Type, Attrs ) ->
	stanza_new( Type, Attrs, [] ).

stanza_new( Type, Attrs, Children ) when in( Type, ?all_presence_types ) ->
	exp_node:new( {?ns_jabber_client, <<"presence">>}, [ {<<"type">>, type2bin(Type)} | Attrs ], Children ).


bin2type( B ) when in( B, ?all_presence_types_bin ) -> erlang:binary_to_existing_atom( B, latin1 );
bin2type( BadArg ) -> error({badarg, presence_type_bin, BadArg}).

type2bin( B ) when in( B, ?all_presence_types ) -> erlang:atom_to_binary( B, latin1 );
type2bin( BadArg ) -> error({badarg, presence_type, BadArg}).


