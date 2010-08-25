
-module(smpp34kit_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	PduGenServer = {smpp34kit_pdugen,
		{smpp34kit_pdugen, start_link, [10001, 50]},
		permanent, 5000, worker, [smpp34kit_pdugen]},

	PduGenSessionSup = {smpp34kit_pdugensession_sup,
		{smpp34kit_pdugensession_sup, start_link, []},
		permanent, infinity, supervisor, [smpp34kit_pdugensession_sup]},

    {ok, { {one_for_one, 5, 10}, [PduGenServer, PduGenSessionSup]}}.

