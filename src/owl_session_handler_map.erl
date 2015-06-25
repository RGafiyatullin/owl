-module (owl_session_handler_map).
-export ([
		new/0,
		subscribe/6, subscribe/5,
		unsubscribe/2,
		get_recepients/2
	]).
-export_type ([
		handler_map/0,
		match_spec/0
	]).

-include_lib("expellee/include/xml.hrl").
-include ("metrics.hrl").

-type fun_xml_element_to_boolean() :: fun( (xml_element()) -> boolean() ).

-type stanza_id() :: binary().
-type match_spec() ::
	  {ns, xml_ns()}
	| {fqn, xml_ns(), xml_ncname()}
	| {fqn_and_id, xml_ns(), xml_ncname(), stanza_id()}.


-record(h, {
		'#module' = ?MODULE :: ?MODULE,
		id :: reference(),
		match :: match_spec(),
		predicates = [] :: [ fun_xml_element_to_boolean() ],
		receiver_pid :: pid(),
		priority :: integer(),
		single_shot :: boolean(),
		timeout_abs :: non_neg_integer()
	}).
-record(hm, {
		'#module' = ?MODULE :: ?MODULE,

		size = 0 :: non_neg_integer(),
		handlers = [] :: [ #h{} ]
	}).

-type hm() :: #hm{}.
-type handler_map() :: hm().

new() ->
	{ok, #hm{}}.

subscribe( MatchSpec, ReceiverPid, Priority, TimeoutAbs, HM ) ->
	subscribe( MatchSpec, [], ReceiverPid, Priority, TimeoutAbs, HM ).

subscribe( MatchSpec, Predicates, ReceiverPid, Priority, TimeoutAbs, HM ) ->
	?duration( 'owl_session_handler_map:subscribe/6',
		subscribe_impl( MatchSpec, Predicates, ReceiverPid, Priority, TimeoutAbs, HM ) ).

unsubscribe( HandlerID, HM ) ->
	?duration( 'owl_session_handler_map:unsubscribe/2',
		unsubscribe_impl( HandlerID, HM ) ).

get_recepients( {NS, NCN, ID}, HM ) ->
	?duration( 'owl_session_handler_map:get_recepients/2',
		get_recepients_impl( {NS, NCN, ID}, HM ) ).




subscribe_impl( MatchSpec, Predicates, ReceiverPid, Priority, TimeoutAbs, HM0 = #hm{ handlers = Handlers0, size = Sz0 } ) ->
	HandlerID = erlang:make_ref(),
	Handler = #h{
			id = HandlerID,
			match = MatchSpec,
			predicates = Predicates,
			receiver_pid = ReceiverPid,
			priority = Priority,
			single_shot = is_integer(TimeoutAbs),
			timeout_abs = TimeoutAbs
		},
	Handlers1 = [ Handler | Handlers0 ],
	Sz1 = Sz0 + 1,
	HM1 = HM0 #hm{ size = Sz1, handlers = Handlers1 },
	{ok, HandlerID, HM1}.

unsubscribe_impl( HandlerID, HM0 = #hm{ handlers = Handlers0, size = Sz0 } ) ->
	case lists:keytake( HandlerID, #h.id, Handlers0 ) of
		false -> {ok, HM0};
		{value, #h{}, Handlers1} ->
			HM1 = HM0 #hm{ size = Sz0 - 1, handlers = Handlers1 },
			{ok, HM1}
	end.


get_recepients_impl( {NS, NCN, ID}, HM0 = #hm{ handlers = Handlers0 } ) ->
	{HandlersToTrigger, SzNext, HandlersKept} =
		?duration('owl_session_handler_map:get_recepients/4[filter]',
			get_recepients_loop( {NS, NCN, ID}, {[], 0, []}, Handlers0 ) ),
	Recepients =
		?duration('owl_session_handler_map:get_recepients/4[sort]',
			[ {P, HID}
				|| #h{ id = HID, receiver_pid = P }
				<- lists:sort(
					fun handler_ordering_fun/2,
					HandlersToTrigger ) ] ),
	HM1 = HM0 #hm{ size = SzNext, handlers = HandlersKept },
	{ ok, Recepients, HM1 }.

handler_ordering_fun( #h{ priority = Left }, #h{ priority = Right } ) -> Left =< Right.


get_recepients_loop( _, Result, [] ) -> Result;

get_recepients_loop( StanzaProps = {_, _, _}, {ToTrigger, Count, ToKeep}, [ Handler | HandlersToCheck ] ) ->
	NextState =
		case check_handler( StanzaProps, Handler ) of
			{false, true} ->
				{ToTrigger, Count + 1, [ Handler | ToKeep ]};

			{true, true} ->
				{[ Handler | ToTrigger ], Count + 1, [ Handler | ToKeep ]};

			{true, false} ->
				{[ Handler | ToTrigger ], Count, ToKeep}
		end,
	get_recepients_loop( StanzaProps, NextState, HandlersToCheck ).

-spec check_handler( { xml_ns(), xml_ncname(), undefined | binary() }, #h{} ) -> { DoesMatch :: boolean(), ShouldKeep :: boolean() }.
check_handler( StanzaProps, #h{ match = Match, single_shot = SingleShot } ) ->
	case {StanzaProps, Match} of
 		{ {NS, NCN, DefinedID}, {fqn_and_id, NS, NCN, DefinedID} } -> {true, not SingleShot};
 		{ {NS, NCN, _}, {fqn, NS, NCN} } -> {true, not SingleShot};
 		{ {NS, _, _}, {ns, NS} } -> {true, not SingleShot};

 		{ _, _ } -> {false, true}
	end.


