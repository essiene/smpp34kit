-module(smpp34kit_pdugensession).
-behaviour(gen_fsm).
-include_lib("smpp34pdu/include/smpp34pdu.hrl").

-export([start_link/1, beginrx/1]).

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

-record(st, {socket, ref, data = <<>>}).


start_link(Socket) ->
    error_logger:info_msg("Starting session~n"),
    gen_fsm:start_link(?MODULE, [Socket], []).

beginrx(Pid) ->
	gen_fsm:send_all_state_event(Pid, beginrx).


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

handle_event(beginrx, State, #st{socket=Socket}=St) ->
	inet:setopts(Socket, [{active, once}]),
	{next_state, State, St};
handle_event(_Event, State, St) ->
    {next_state, State, St}.

handle_sync_event(Event, _From, State, St) ->
    {reply, {illegal, Event}, State, St}.

handle_info({tcp, Socket, Data}, State, #st{socket=Socket, data=Data0}=St) ->
	Data1 = <<Data0/binary, Data/binary>>,
	{_, PduList, Rest} = smpp34pdu:unpack(Data1),
	log_pdu(PduList),
	inet:setopts(Socket, [{active, once}]),
	{next_state, State, St#st{data=Rest}};
handle_info({tcp_error, Socket, Reason}, _, #st{socket=Socket}=St) ->
	error_logger:error_msg("Error in connection: {~p, ~p}~n", [Socket, Reason]),
	{stop, normal, St};
handle_info({tcp_closed, Socket}, _, #st{socket=Socket}=St) ->
	error_logger:info_msg("Connection closed by client~n"),
	{stop, normal, St};
handle_info(_Info, State, St) ->
    {next_state, State, St}.

terminate(_Reason, _St, #st{socket=Socket}) ->
    gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, St, _Extra) ->
    {next_state, State, St}.


log_pdu([]) ->
	ok;
log_pdu([Pdu|Rest]) ->
	log_pdu(Pdu),
	log_pdu(Rest);
log_pdu(Pdu) ->
	error_logger:info_msg("==>~p~n",[Pdu]).
