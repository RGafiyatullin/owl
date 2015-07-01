-module (owl_session_service_muc_client_occupant).
% -behaviour (gen_server).

-export ([
		start_link_sup/1
	]).
-export ([
		init/1
	]).

-spec start_link_sup( MucClientService :: pid() ) ->
start_link_sup( MucClientService ) when is_pid( MucClientService ) ->
	supervisor:start_link( ?MODULE, { sup, MucClientService } ).


init( {sup, MucClientService} ) ->
	init_sup( MucClientService );
init( {occupant, MucClientService, OccupantJid} ) ->
	{stop, {not_implemented, {MucClientService, OccupantJid}}}.


init_sup( MucClientService ) ->
	{ok, { {simple_one_for_one, 0, 1},
			[
				{ occupant, {}, temporary, 5000, worker, [ ?MODULE ] }
			] }}.

