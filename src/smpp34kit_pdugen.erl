-module(smpp34kit_pdugen).
-include_lib("smpp34pdu/include/smpp34pdu.hrl").
-behaviour(gen_listener_tcp).


-export([
        start_link/2
    ]).

-export([
        init/1,
        handle_accept/2,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
    ]).

start_link(Port, Backlog) ->
    gen_listener_tcp:start_link({local, ?MODULE}, ?MODULE, [Port, Backlog], []).

init([Port, Backlog]) -> 
	{ok, 
		{Port, [binary, inet, {active, false}, 
						{backlog, Backlog},
                        {reuseaddr, true}]
                },
                nil
     }.

handle_accept(Sock, State) -> 
    smpp34kit_pdugensession_sup:start_child(Sock),
    {noreply, State}.

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

