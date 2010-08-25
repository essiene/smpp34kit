-module(smpp34kit_pdugensession_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Socket) ->
	supervisor:start_child(?MODULE, [Socket]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Session = {smpp34kit_pdugensession,
		{smpp34kit_pdugensession, start_link, []},
		temporary, 5000, worker, [smpp34kit_pdugensession]},

    {ok, { {simple_one_for_one, 5, 10}, [Session]}}.

