% Realiable statefull writer, it manages Reader-proxies sends heatbeats and receives acknacks
-module(rtps_full_writer).

-export([
    start_link/1,
    on_change_available/2,
    on_change_removed/2, new_change/2,
    get_matched_readers/1,
    get_cache/1,
    update_matched_readers/2,
    matched_reader_add/2,
    matched_reader_remove/2,
    is_acked_by_all/2,
    receive_acknack/2,
    receive_nackfrag/2,
    flush_all_changes/1
]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("kernel/include/logger.hrl").
-include("../include/rtps_structure.hrl").
-include("../include/rtps_constants.hrl").

-define(DEFAULT_WRITE_PERIOD, 1000).
-define(DEFAULT_HEARTBEAT_PERIOD, 1000).
-define(DEFAULT_NACK_RESPONCE_DELAY, 200).

-define(DATA_SUBMSG_OVERHEAD, 28).
-define(DATAFRAG_SUBMSG_OVERHEAD, 36).
-define(NEEDS_FRAGMENTATION(CurrentPayload, Payload, MTU),
        (size(CurrentPayload) + size(Payload) + ?DATA_SUBMSG_OVERHEAD > MTU)).
-define(CAN_FRAGMENT(CurrentPayload, MTU),
        (MTU - size(CurrentPayload) - ?DATAFRAG_SUBMSG_OVERHEAD > 0)).

-record(state,
        {participant = #participant{},
         entity = #endPoint{},
         datawrite_period = ?DEFAULT_WRITE_PERIOD div 10, % default at 1000
         heatbeat_period = ?DEFAULT_HEARTBEAT_PERIOD div 10, % default at 1000
         nackResponseDelay = ?DEFAULT_NACK_RESPONCE_DELAY div 10, % default at 200
         heatbeat_count = 1,
         nackSuppressionDuration = 0,
         push_mode = true,
         history_cache,
         reader_proxies = #{} :: #{#guId{} => #reader_proxy{}},
         last_sequence_number = 0}).

%API
start_link({Participant, WriterConfig}) ->
    gen_server:start_link(?MODULE, {Participant, WriterConfig}, []).

new_change(Name, Data) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:call(Pid, {new_change, Data}).

on_change_available(Name, ChangeKey) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:cast(Pid, {on_change_available, ChangeKey}).

on_change_removed(Name, ChangeKey) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:cast(Pid, {on_change_removed, ChangeKey}).

%Adds new locators if missing, removes old locators not specified in the call.
update_matched_readers(Name, R) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:cast(Pid, {update_matched_readers, R}).

get_matched_readers(Name) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:call(Pid, get_matched_readers).

matched_reader_add(Name, R) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:cast(Pid, {matched_reader_add, R}).

matched_reader_remove(Name, R) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:cast(Pid, {matched_reader_remove, R}).

is_acked_by_all(Name,ChangeKey) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:call(Pid, {is_acked_by_all,ChangeKey}).

receive_acknack(Name, Acknack) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:cast(Pid, {receive_acknack, Acknack}).

receive_nackfrag(Name, NackFrag) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:cast(Pid, {receive_nackfrag, NackFrag}).

get_cache(Name) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:call(Pid, get_cache).

flush_all_changes(Name) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:call(Pid, flush_all_changes).

% callbacks
init({Participant, #endPoint{guid = GUID} = WriterConfig}) ->
    pg:join(GUID, self()),
    %io:format("~p.erl STARTED!\n",[?MODULE]),
    State =
        #state{participant = Participant,
               entity = WriterConfig,
               history_cache = {cache_of, GUID}},
    rtps_history_cache:set_listener({cache_of, GUID}, {?MODULE, GUID}),

    erlang:send_after(10, self(), heartbeat_loop),
    erlang:send_after(10, self(), write_loop),
    {ok, State}.

handle_call({new_change, Data}, _, State) ->
    {Change, NewState} = h_new_change(Data, State),
    {reply, Change, NewState};
handle_call(get_cache, _, State) ->
    {reply, State#state.history_cache, State};
handle_call({is_acked_by_all,ChangeKey}, _, State) ->
    {reply, h_is_acked_by_all(ChangeKey,State), State};
handle_call(get_matched_readers, _, State) ->
    {reply,  State#state.reader_proxies, State};
handle_call(flush_all_changes, _, State) ->
    {reply, h_flush_all_changes(State), State}.

handle_cast({on_change_available, ChangeKey}, S) ->
    {noreply, h_on_change_available(ChangeKey, S)};
handle_cast({on_change_removed, ChangeKey}, S) ->
    {noreply, h_on_change_removed(ChangeKey, S)};
handle_cast({update_matched_readers, Proxies}, State) ->
    {noreply, h_update_matched_readers(Proxies, State)};
handle_cast({matched_reader_add, Proxy}, State) ->
    {noreply, h_matched_reader_add(Proxy, State)};
handle_cast({matched_reader_remove, Guid}, State) ->
    {noreply, h_matched_reader_remove(Guid, State)};
handle_cast({receive_acknack, Acknack}, State) ->
    {noreply, h_receive_acknack(Acknack, State)};
handle_cast({receive_nackfrag, NackFrag}, State) ->
    {noreply, h_receive_nackfrag(NackFrag, State)}.

handle_info(heartbeat_loop, State) ->
    {noreply, heartbeat_loop(State)};
handle_info(write_loop, State) ->
    {noreply, write_loop(State)}.

%callback helpers

all_changes_are_acknowledged(ReaderChanges) ->
    lists:all(fun
            (#change_for_reader{status = acknowledged}) -> true;
            (_) -> false
        end,
        maps:values(ReaderChanges)).

send_heartbeat_to_reader(#guId{prefix = GuidPrefix} = GUID,
                          HB,
                          ReaderGUID,
                          #reader_proxy{changes_for_reader = C4R,
                                        unicastLocatorList = [L | _]}) ->
    case all_changes_are_acknowledged(C4R) of
        true -> ok;
        false ->
            NewHB = HB#heartbeat{readerGUID = ReaderGUID},
            SUB_MSG_LIST = [rtps_messages:serialize_heatbeat(NewHB)],
            Datagram = rtps_messages:build_message(GuidPrefix, SUB_MSG_LIST),
            rtps_gateway:send(GUID, Datagram, L#locator.ip, L#locator.port)
    end.

build_heartbeat(GUID, Cache, Count) ->
    MinSN = rtps_history_cache:get_min_seq_num(Cache),
    MaxSN = rtps_history_cache:get_max_seq_num(Cache),
    #heartbeat{writerGUID = GUID,
               min_sn = MinSN,
               max_sn = MaxSN,
               count = Count,
               final_flag = 0,
               readerGUID = ?GUID_UNKNOWN}.

% Must send the first HB with min 1 and max 0 to allow first MSG to be considered,
% This triggers an acknack response with final_flag=1 which means the next data is really requested
% This is already done by the rtps_history_cache:get_min_seq_num() and rtps_history_cache:get_max_seq_num()
send_heartbeat(#state{entity = #endPoint{guid = GUID},
                      history_cache = C,
                      heatbeat_count = Count,
                      reader_proxies = RP}) ->
    HB = build_heartbeat(GUID, C, Count),
    maps:foreach(fun(ReaderID, Proxy) ->
            send_heartbeat_to_reader(GUID, HB, ReaderID, Proxy)
        end,
        RP).

heartbeat_loop(#state{heatbeat_period = HP, heatbeat_count = C} = S) ->
    send_heartbeat(S),
    erlang:send_after(HP, self(), heartbeat_loop),
    S#state{heatbeat_count = C + 1}.

send_selected_changes([], _, _, _,#reader_proxy{changes_for_reader = CR}) -> CR;
send_selected_changes(RequestedSN,
                      Guid,
                      HC,
                      ReaderID,
                      #reader_proxy{unicastLocatorList = [L | _],
                                    changes_for_reader = C4R} = RP) ->
    ToSend = [{SN, rtps_history_cache:get_change(HC, {Guid, SN})} || SN <- RequestedSN],
    ValidToSend = [{SN,C} || {SN,C} <- ToSend, C /= not_found],
    {RTPSMessage, NewC4R, Action} = build_rtps_message(Guid, HC, ValidToSend, C4R, ReaderID),
    rtps_gateway:send(Guid, RTPSMessage, L#locator.ip, L#locator.port),
    case Action of
        no_action ->
            NewC4R;
        {send_more_fragments, SN} ->
            send_selected_changes([SN], Guid, HC, ReaderID, RP#reader_proxy{changes_for_reader = NewC4R})
    end.

build_rtps_message(Guid, HC, ValidToSend, ChangesForReader, ReaderID) ->
    MTU = rtps_network_utils:get_mtu(),
    UDP_MTU = MTU - 20 - 8, % 20 for IP and 8 for UDP header
    RTPSHeader = rtps_messages:header(Guid#guId.prefix),
    InfoTimestamp = rtps_messages:serialize_info_timestamp(),
    BaseMessage = <<RTPSHeader/binary, InfoTimestamp/binary>>,
    lists:foldl(fun(C, Acc) ->
                    build_submessage(Guid, HC, C, Acc, UDP_MTU, ReaderID)
                end,
                {BaseMessage, ChangesForReader, no_action},
                ValidToSend).

build_submessage(_, _,{SN, #cacheChange{data = Data} = CC}, {BinAcc, C4RMap, _}, MTU, ReaderID)
    when is_record(Data, sedp_disc_endpoint_data)
        orelse
        (is_binary(Data) andalso (size(BinAcc) + size(Data) + ?DATA_SUBMSG_OVERHEAD) =< MTU)
->
    % We can normally serialize the DATA_SUBMESSAGE
    Change4R = maps:get(SN, C4RMap),
    SubMsg = rtps_messages:serialize_data(ReaderID#guId.entityId, CC),
    NewBin = <<BinAcc/binary, SubMsg/binary>>,
    % mark all sent requests as "unacknowledged"
    % (skipping the "UNDERWAY" status) just for simplicity
    NewC4R = maps:put(SN, Change4R#change_for_reader{status = unacknowledged}, C4RMap),
    {NewBin, NewC4R, no_action};
build_submessage(Guid, HC, {SN, #cacheChange{data = Data} = CC}, {BinAcc, C4RMap, _}, MTU, ReaderID)
    when is_binary(Data) andalso
         ?NEEDS_FRAGMENTATION(BinAcc, Data, MTU) andalso
         ?CAN_FRAGMENT(BinAcc, MTU)
->
    % We have a binary which is too big to send with the current MTU,
    % we need to split it into a map
    Change4R = maps:get(SN, C4RMap),
    FragSize = MTU - size(BinAcc) - ?DATAFRAG_SUBMSG_OVERHEAD,
    {FragmentMap, FragmentedChange4R} = fragment_data(Data, FragSize, Change4R),
    NewCC = CC#cacheChange{data = FragmentMap},
    rtps_history_cache:fragmented_change(HC, {Guid, SN}, FragmentMap),
    {SubMsg, NewChange4R} = build_datafrag_submessage(NewCC, FragmentedChange4R, ReaderID),
    NewBin = <<BinAcc/binary, SubMsg/binary>>,
    NewC4RMap = maps:put(SN, NewChange4R, C4RMap),
    {NewBin, NewC4RMap, {send_more_fragments, SN}};
build_submessage(_, _, {SN, #cacheChange{data = Data} = CC}, {BinAcc, C4RMap, _}, MTU, ReaderID)
    when is_map(Data)
->
    #change_for_reader{
        fragment_size = FragmentSize} = Change4R = maps:get(SN, C4RMap),
    AvailableSpace = MTU - size(BinAcc) - ?DATAFRAG_SUBMSG_OVERHEAD,
    case FragmentSize > AvailableSpace of
        true ->
            ?LOG_ERROR("No space left to send any datafrag: available space ~p, fragment size ~p~n",
                       [AvailableSpace, FragmentSize]),
            {BinAcc, C4RMap, {send_more_fragments, SN}};
        false ->
            % We can send one fragment, we could send more then one, maybe,
            % but we already maximize the fragment size to fill the MTU.
            {SubMsg, NewChange4R} = build_datafrag_submessage(CC, Change4R, ReaderID),
            NewFragStates = NewChange4R#change_for_reader.fragments_state,
            NextAction = case lists:all(fun(Status) -> Status == sent end, maps:values(NewFragStates)) of
                true ->
                    no_action;
                false ->
                    {send_more_fragments, SN}
            end,
            NewBin = <<BinAcc/binary, SubMsg/binary>>,
            NewC4RMap = maps:put(SN, NewChange4R, C4RMap),
            {NewBin, NewC4RMap, NextAction}
    end;
build_submessage(_, _, {SN, _}, {BinAcc, C4RMap, Action}, _, _) ->
    ?LOG_INFO("Writer exausted MTU space, skipping send of data message ~p", [SN]),
    {BinAcc, C4RMap, Action}.

build_datafrag_submessage(CC, Change4R, ReaderID) ->
    #change_for_reader{
        fragments_state = FragmentStates,
        sample_size = SampleSize,
        fragment_size = FragmentSize} = Change4R,
    % For simplicity we assume Fragment size is already filling the MTU
    % and we don't need to send more than one fragment
    {From, To} = Range = calc_fragment_range(FragmentStates),
    SubMsg = case Range of
        {0,0} ->
            % The range might be 0-0 it means we sent everything,
            % we can skip it and wait for a nackfrag
            <<>>;
        _ ->
            rtps_messages:serialize_data_frag(ReaderID#guId.entityId, CC, Range,
                                               SampleSize, FragmentSize)
    end,
    NewStates = maps:map(fun(SN, Status) ->
            case  From =< SN andalso SN =< To of
                true -> sent;
                false -> Status
            end
        end, FragmentStates),

    NewC4R = Change4R#change_for_reader{status = unacknowledged,
                                        fragments_state = NewStates},
    {SubMsg, NewC4R}.

calc_fragment_range(FragmentStates) ->
    % For simplicity we assume Fragment size is already filling the MTU
    % and we don't need to send more than one fragment,
    % TODO:
    % We should check the available space and extend the range if there is space
    % but then we shoud jump sections of fragments that are already sent
    FragmentToBeSent = [SN || {SN, Status} <- maps:to_list(FragmentStates),
                         Status /= received andalso Status /= sent],
    case FragmentToBeSent of
        [] -> {0,0};
        _ ->
            StartingFragmentNumber = lists:min(FragmentToBeSent),
            FragmentsInSubmessage = 1,
            {StartingFragmentNumber, StartingFragmentNumber + FragmentsInSubmessage - 1}
    end.

send_changes(Filter, Guid, HC, RP) ->
    maps:map(fun
        (ReaderID, #reader_proxy{changes_for_reader = CR} = P) ->
            RequestedKeys = [K || {K, #change_for_reader{status = S}} <- maps:to_list(CR), S == Filter],
            NewCR = send_selected_changes(RequestedKeys, Guid, HC, ReaderID, P),
            P#reader_proxy{changes_for_reader = NewCR}
        end,
        RP).

write_loop(#state{entity = #endPoint{guid = Guid},
                  history_cache = HC,
                  datawrite_period = P,
                  reader_proxies = RP,
                  push_mode = true} =
               S) ->
    erlang:send_after(P, self(), write_loop),
    ProxiesPushed = send_changes(unsent, Guid, HC, RP),
    S#state{reader_proxies = send_changes(requested, Guid, HC, ProxiesPushed)};
write_loop(#state{entity = #endPoint{guid = Guid},
                  history_cache = HC,
                  datawrite_period = P,
                  reader_proxies = RP} =
               S) ->
    erlang:send_after(P, self(), write_loop),
    S#state{reader_proxies = send_changes(requested, Guid, HC, RP)}.

h_new_change(D,
             #state{last_sequence_number = Last_SN,
                    entity = E,
                    history_cache = _C} =
                 S) ->
    SN = Last_SN + 1,
    Change =
        #cacheChange{kind = alive,
                     writerGuid = E#endPoint.guid,
                     instanceHandle = 0,
                     sequenceNumber = SN,
                     data = D},
    {Change, S#state{last_sequence_number = SN}}.

h_update_matched_readers(ProxyToMatch, #state{reader_proxies = RP, history_cache = C} = S) ->
    Current_GUIDS = maps:keys(RP),
    Valid_GUIDS = maps:keys(ProxyToMatch),
    KeyToRemove = Current_GUIDS -- Valid_GUIDS,
    ProxiesToKeep = lists:foldl(fun(GUID, Acc) ->
            maps:remove(GUID, Acc)
        end,
        RP,
        KeyToRemove),
    NewProxies = lists:foldl(fun(GUID, Acc) ->
            maps:remove(GUID, Acc)
        end,
        ProxyToMatch,
        Current_GUIDS),
    Changes = rtps_history_cache:get_all_changes(C),
    % add cache changes to the unsent list for the new added locators
    NewProxiesReady = maps:map(
        fun(_, Proxy) ->
                setup_reader_proxy(Changes, Proxy)
        end,
        NewProxies),
    S#state{reader_proxies = maps:merge(ProxiesToKeep, NewProxiesReady)}.

h_matched_reader_add({ReaderGUID, Proxy},
                     #state{entity = #endPoint{guid = GUID},
                            reader_proxies = RP,
                            history_cache = C,
                            heatbeat_count = Count} =
                         S) ->
    io:format("matched_reader_add: ~p~n", [ReaderGUID]),
    Changes = rtps_history_cache:get_all_changes(C),
    NewProxy = setup_reader_proxy(Changes, Proxy),
    HB = build_heartbeat(GUID, C, Count),
    send_heartbeat_to_reader(GUID, HB, ReaderGUID, NewProxy),
    S#state{
        reader_proxies = RP#{ReaderGUID => NewProxy},
        heatbeat_count = Count + 1}.

h_matched_reader_remove(Guid, #state{reader_proxies = RP} = S) ->
    S#state{reader_proxies = maps:remove(Guid, RP)}.

setup_reader_proxy(Changes, Proxy) ->
    ChangesForReaders = lists:foldl(
        fun
            (#cacheChange{sequenceNumber = SN, data = Data}, Acc) when is_map(Data) ->
                FirstChunkSize = size(maps:get(1, Data)),
                SampleSize = lists:foldl(fun(B, Size) -> Size + size(B) end, 0, maps:values(Data)),
                FragmentStates = #{ K => unknown || K <- maps:keys(Data)},
                maps:put(SN, #change_for_reader{
                        status = unacknowledged,
                        is_fragmented = true,
                        sample_size = SampleSize,
                        fragment_size = FirstChunkSize,
                        fragments_state = FragmentStates
                    },
                    Acc);
            (#cacheChange{sequenceNumber = SN}, Acc) ->
                maps:put(SN, #change_for_reader{status = unacknowledged}, Acc)
        end,
        #{},
        Changes),
    Proxy#reader_proxy{changes_for_reader = ChangesForReaders}.

is_acked_by_reader(SN, #reader_proxy{changes_for_reader = Changes}) ->
    %[ io:format("~p with key = ~p\n",[S,Key]) || #change_for_reader{change_key=Key, status = S}=C <- Changes],
    case maps:get(SN, Changes, #change_for_reader{status = unacknowledged}) of
        #change_for_reader{status = acknowledged} -> true;
        _ -> false
    end.

h_is_acked_by_all(ChangeKey, #state{reader_proxies = RP}) ->
    lists:all(fun(Proxy) -> is_acked_by_reader(ChangeKey,Proxy) end, RP).

add_change_to_proxies(SN, Proxies, Push) ->
    maps:map(fun(_, Proxy) ->
        add_change_to_proxy(SN, Proxy, Push)
    end, Proxies).

add_change_to_proxy(SN, Proxy, Push) ->
    Status = case Push of
        true -> unsent;
        false -> unacknowledged
    end,
    ReaderChange = #change_for_reader{status = Status},
    ChangeMap = (Proxy#reader_proxy.changes_for_reader)#{SN => ReaderChange},
    Proxy#reader_proxy{changes_for_reader = ChangeMap}.

h_on_change_available({_, SN},
                      #state{entity = #endPoint{guid = Guid},
                             history_cache = HC,
                             reader_proxies = RP,
                             push_mode = Push} =
                          S) ->
    ReaderProxies = add_change_to_proxies(SN, RP, Push),
    % instantly send changes that are to be pushed
    ProxiesPushed = send_changes(unsent, Guid, HC, ReaderProxies),
    S#state{reader_proxies = ProxiesPushed}.

rm_change_from_proxies(SN, Proxies) ->
    maps:map(fun(_, Proxy) ->
        rm_change_from_proxy(SN, Proxy)
    end, Proxies).

rm_change_from_proxy(SN, Proxy) ->
    NewChangeList = maps:remove(SN, Proxy#reader_proxy.changes_for_reader),
    Proxy#reader_proxy{changes_for_reader = NewChangeList}.

h_on_change_removed(Key, #state{history_cache = _C, reader_proxies = RP} = S) ->
    S#state{reader_proxies = rm_change_from_proxies(Key, RP)}.

update_for_acknack(RGUID, Missed, FinalFlag, #state{reader_proxies = RP} = S) ->
    #reader_proxy{ready = IsReady,
                  changes_for_reader = Changes} = Proxy = maps:get(RGUID, RP),
    NewChangeMap =
        maps:map(fun(SN, C) ->
                    case lists:member(SN, Missed) of
                        true ->
                            C#change_for_reader{status = requested};
                        false ->
                            case SN < lists:min(Missed) of
                                true ->
                                    C#change_for_reader{status = acknowledged};
                                false ->
                                    case SN > lists:max(Missed) of
                                        true ->
                                            C#change_for_reader{status = unacknowledged};
                                        false ->
                                            C
                                    end
                            end
                    end
                end,
                Changes),
    % a remote reader is considered ready to receive only after it sent an acknack with a final flag
    Readiness = not IsReady and (FinalFlag == 1),
    NewProxy = Proxy#reader_proxy{ready = Readiness,
                                  changes_for_reader = NewChangeMap},
    S#state{reader_proxies = maps:put(RGUID, NewProxy, RP)}.

update_for_nack_frag(RGUID, SN, Missing, #state{reader_proxies = RP} = S) ->
    #reader_proxy{changes_for_reader = Changes} = Proxy = maps:get(RGUID, RP),
    #change_for_reader{
        is_fragmented = IsFragmented,
        fragments_state = FragmentStates} = C = maps:get(SN, Changes),
    SmallerMissing = lists:min(Missing),
    NewChange = case IsFragmented of
        true ->
            NewFragmentStates = maps:map(fun(N, Status) ->
                    case lists:member(N, Missing) of
                        true -> nacked;
                        false -> case N < SmallerMissing of
                            true -> received;
                            false -> Status
                        end
                    end
                end, FragmentStates),
            C#change_for_reader{fragments_state = NewFragmentStates};
        false ->
            C
    end,
    NewChangeMap = Changes#{SN => NewChange},
    NewProxy = Proxy#reader_proxy{changes_for_reader = NewChangeMap},
    S#state{reader_proxies = maps:put(RGUID, NewProxy, RP)}.

h_receive_acknack(#acknack{readerGUID = RGUID, final_flag = FF, sn_range = SNRange},
                  #state{reader_proxies = RP} = S) ->
    Range = case is_integer(SNRange) of true -> [SNRange]; false -> SNRange end,
    case maps:is_key(RGUID, RP) of
        true -> update_for_acknack(RGUID, Range, FF, S);
        false -> S
    end.

h_receive_nackfrag(#nackfrag{readerGUID = RGUID, sn = SN, missing_fragments = Missing},
                   #state{reader_proxies = RP} = S) ->
    case maps:is_key(RGUID, RP) of
        true -> update_for_nack_frag(RGUID, SN, Missing, S);
        false -> S
    end.

h_flush_all_changes(#state{entity = #endPoint{guid = #guId{prefix = Prefix}},
                       history_cache = HC,
                       datawrite_period = _P,
                       reader_proxies = RP}) ->
send_changes(unsent, Prefix, HC, RP).

fragment_data(Data, ChunkSize, Change4R) ->
    % The first fragment contains the representation id of the whole sample
    DataWithEncoding = rtps_messages:add_rappresentation_id(Data),
    <<FirstChunk:ChunkSize/binary, Rest/binary>> = DataWithEncoding,
    FragmentsList = rec_fragment_data(Rest, ChunkSize, 2, [{1, FirstChunk}]),
    FragmentStates = maps:from_list([{SN, unknown} || {SN, _} <- FragmentsList]),
    NewChange4R = Change4R#change_for_reader{is_fragmented = true,
                                             sample_size = size(DataWithEncoding),
                                             fragment_size = ChunkSize,
                                             fragments_state = FragmentStates},
    {maps:from_list(FragmentsList), NewChange4R}.

rec_fragment_data(Data, ChunkSize, Number, Chunks) when size(Data) < ChunkSize ->
    [{Number, Data} | Chunks];
rec_fragment_data(Data, ChunkSize, Number, Chunks) ->
    <<Chunk:ChunkSize/binary, Rest/binary>> = Data,
    rec_fragment_data(Rest, ChunkSize, Number + 1, [{Number, Chunk} | Chunks]).
