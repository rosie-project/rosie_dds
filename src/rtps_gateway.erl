% Just a process to hold an UDP socket for sending packets
-module(rtps_gateway).
-export([
    start_link/0,
    send/4
]).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3]).

-include_lib("kernel/include/logger.hrl").
-include("../include/rtps_structure.hrl").

-record(state, {participant = #participant{}, socket}).

%API
start_link() ->
    gen_server:start_link(?MODULE, #state{}, []).

send(Guid, Data, IP, Port) ->
    [Pid | _] = pg:get_local_members(rtps_gateway),
    gen_server:cast(Pid, {send, Guid, Data, IP, Port}).

% callbacks
init(State) ->
    pg:join(rtps_gateway, self()),
    %rtps_network_utils:wait_dhcp(5000),
    LocalInterface = rtps_network_utils:get_local_ip(),
    {ok, S} = gen_udp:open(0,[{ip, LocalInterface}, binary, {active, true}]),
    {ok, State#state{socket = S}}.

% handle_call({send,{Datagram,Dst}}, _, #state{socket=S}=State) ->
%         {reply,gen_udp:send(S,Dst,Datagram),State};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({send, Guid, Datagram, IP, Port}, State) ->
    case dispatch_rtps_message(Guid, Datagram, IP, Port, State) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_ERROR("[GATEWAY]: failed sending to: ~p\n",[{IP, Port}]),
            ?LOG_ERROR("[GATEWAY]: reason of failure: ~p\n",[Reason])
    end,
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

dispatch_rtps_message(Guid, Datagram, IP, Port, #state{socket = S}) ->
    Options = #{
        sender => Guid,
        dst => {IP, Port}
    },
    case application:get_env(rosie_dds, gateway_fun) of
        undefined ->
            gen_udp:send(S, {IP, Port}, Datagram);
        {ok, {Module, Fun}} ->
            Module:Fun({Datagram, Options});
        {ok, F} when is_function(F) ->
            F({Datagram, Options})
    end.
