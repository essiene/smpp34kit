-module(smpp34kit_pdugensession).
-behaviour(gen_fsm).
-include_lib("smpp34pdu/include/smpp34pdu.hrl").

-export([start_link/1]).

-export([
		idle/2,
		idle/3,
        active/2,
        active/3
    ]).


-export([
        init/1,
        handle_event/3,
        handle_sync_event/4,
        handle_info/3,
        terminate/3,
        code_change/4
    ]).

-record(st, {socket, ref}).


start_link(Socket) ->
    error_logger:info_msg("Starting session~n"),
    gen_fsm:start_link(?MODULE, [Socket], []).


idle({timeout, Ref, start}, #st{ref=Ref}=St) ->
	Ref1 = gen_fsm:start_timer(1000, heartbeat),
	{next_state, active, St#st{ref=Ref1}};
idle(_E, St) ->
	{next_state, idle, St}.
	
active({timeout, Ref, heartbeat}, #st{ref=Ref, socket=Socket}=St) ->
	PduBody = #bind_transceiver_resp{
				system_id="smpp34kit_genpdu", 
				sc_interface_version=16#34},
	gen_tcp:send(Socket, smpp34pdu:pack(?ESME_ROK, 0, PduBody)),
	Ref1 = gen_fsm:start_timer(1000, heartbeat),
	{next_state, active, St#st{ref=Ref1}};
active(_Event, St) ->
    {next_state, active, St}.

idle(Event, _From, St) ->
	{reply, {illegal, Event}, idle, St}.

active(Event, _From, St) ->
    {reply, {illegal, Event}, active, St}.


init([Socket]) ->
    ok = inet:setopts(Socket, [{active, once}]),
	Ref = gen_fsm:start_timer(3000, start),
    State = #st{socket=Socket, ref=Ref},
    {ok, idle, State}.

handle_event(_Event, State, St) ->
    {next_state, State, St}.

handle_sync_event(Event, _From, State, St) ->
    {reply, {illegal, Event}, State, St}.

handle_info(_Info, State, St) ->
    {next_state, State, St}.

terminate(_Reason, _St, #st{socket=Socket}) ->
    gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, St, _Extra) ->
    {next_state, State, St}.
