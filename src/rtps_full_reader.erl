% Reliable StateFull RTPS reader with WriterProxies, receives heartbits and sends acknacks
-module(rtps_full_reader).

-export([
    start_link/1,
    matched_writer_add/2,
    matched_writer_remove/2,
    update_matched_writers/2,
    receive_data/2,
    receive_gap/2,
    get_cache/1,
    receive_heartbeat/2
]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/logger.hrl").
-include("../include/rtps_structure.hrl").
-include("../include/rtps_constants.hrl").
-record(state,
        {participant = #participant{},
         entity = #endPoint{},
         history_cache,
         writer_proxies = #{} :: #{#guId{} => #writer_proxy{}},
         heartbeatResponseDelay = 20, %default should be 500
         heartbeatSuppressionDuration = 0,
         acknack_count = 0,
         nackfrag_count = 0}).

%API
start_link({Participant, ReaderConfig}) ->
    State = #state{participant = Participant, entity = ReaderConfig},
    gen_server:start_link(?MODULE, State, []).

get_cache(Name) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:call(Pid, get_cache).

receive_heartbeat(Pid, HB) when is_pid(Pid) ->
    gen_server:cast(Pid, {receive_heartbeat, HB});
receive_heartbeat(Name, HB) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:cast(Pid, {receive_heartbeat, HB}).

update_matched_writers(Name, Proxies) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:cast(Pid, {update_matched_writers, Proxies}).

matched_writer_add(Name, W) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:cast(Pid, {matched_writer_add, W}).

matched_writer_remove(Name, W) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:cast(Pid, {matched_writer_remove, W}).

receive_data(Pid, Data) when is_pid(Pid) ->
    gen_server:cast(Pid, {receive_data, Data});
receive_data(Name, Data) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:cast(Pid, {receive_data, Data}).

receive_gap(Name, Gap) ->
    [Pid | _] = pg:get_local_members(Name),
    gen_server:cast(Pid, {receive_gap, Gap}).

% callbacks
init(#state{entity = E} = State) ->
    %io:format("~p.erl STARTED!\n",[?MODULE]),
    pg:join(E#endPoint.guid, self()),
    pg:join(rtps_readers, self()),
    {ok, State#state{history_cache = {cache_of, E#endPoint.guid}}}.

handle_call(get_cache, _, State) ->
    {reply, State#state.history_cache, State};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({receive_data, {_, _, #data_frag{}} = DataFrag}, State) ->
    case message_matches_proxy(DataFrag, State) of
        true ->
            {noreply, h_receive_data_frag(DataFrag, State)};
        false ->
            {noreply, State}
    end;
handle_cast({receive_data, {_, _, Payload} = Data}, State)
when is_binary(Payload); is_record(Payload, sedp_disc_endpoint_data) ->
    case message_matches_proxy(Data, State) of
        true ->
            {noreply, h_receive_data(Data, State)};
        false ->
            {noreply, State}
    end;
handle_cast({receive_gap, Gap}, State) ->
    case message_matches_proxy(Gap, State) of
        true ->
            {noreply, h_receive_gap(Gap, State)};
        false ->
            {noreply, State}
    end;
handle_cast({receive_heartbeat, HB}, State) ->
    case message_matches_proxy(HB, State) of
        true ->
            {noreply, h_receive_heartbeat(HB, State)};
        false ->
            {noreply, State}
    end;
handle_cast({update_matched_writers, Proxies}, S) ->
    {noreply, h_update_matched_writers(Proxies, S)};
handle_cast({matched_writer_add, Proxy}, S) ->
    {noreply, h_matched_writer_add(Proxy, S)};
handle_cast({matched_writer_remove, R}, S) ->
    {noreply, h_matched_writer_remove(R, S)};
handle_cast(Cast, State) ->
    error({unexpected_cast, Cast}),
    {noreply, State}.

handle_info({send_acknack_if_needed, {WGUID, FF}}, State) ->
    case message_matches_proxy(WGUID, State) of
        true ->
            {noreply, h_send_acknack_if_needed(WGUID, FF, State)};
        false ->
            {noreply, State}
    end;
handle_info({send_nackfrag_if_needed, WGUID}, State) ->
    case message_matches_proxy(WGUID, State) of
        true ->
            {noreply, h_send_nackfrag_if_needed(WGUID, State)};
        false ->
            {noreply, State}
    end;
handle_info(_, State) ->
    {noreply, State}.

%helpers
data_to_cache_change({Writer, SN, Data}) ->
    #cacheChange{writerGuid = Writer,
                 sequenceNumber = SN,
                 data = Data}.

h_update_matched_writers(Proxies, #state{writer_proxies = WP} = S) ->
    CurrentProxyes = maps:keys(WP),
    NewProxies = maps:filter(
        fun(Key, _) -> not lists:member(Key, CurrentProxyes) end,
        Proxies),
    S#state{writer_proxies = maps:merge(WP, NewProxies)}.

h_matched_writer_add({Guid, Proxy}, #state{writer_proxies = WP} = S) ->
    S#state{writer_proxies = maps:put(Guid, Proxy, WP)}.

h_matched_writer_remove(Guid, #state{writer_proxies = WP} = S) ->
    S#state{writer_proxies = maps:remove(Guid, WP)}.

missing_changes_update(Changes, Min, Max) ->
    NewChanges =
        maps:from_list([
            {SN, #change_from_writer{status = missing}}
            || SN <- lists:seq(Min, Max), not maps:is_key(SN, Changes)]),
    maps:merge(Changes, NewChanges).

gen_change_if_not_there(LastSN, ToBeMarked) ->
    case lists:member(LastSN, [SN || {SN, _} <- ToBeMarked]) of
        true -> [];
        false -> [{LastSN, #change_from_writer{}}]
    end.

lost_changes_update(Changes, FirstSN, LastSN) ->
    {ToBeMarked, Others} = lists:partition(
        fun({SN, #change_from_writer{status = S}}) ->
            ((S == unknown) or (S == missing)) and (SN < FirstSN)
        end,
        maps:to_list(Changes)),
    LastLostChange = case LastSN < FirstSN of
        true -> gen_change_if_not_there(LastSN, ToBeMarked);
        false -> []
    end,
    maps:from_list(
        Others ++
        [{SN, C#change_from_writer{status = lost}} || {SN,C} <- ToBeMarked ++ LastLostChange]).

h_receive_heartbeat(#heartbeat{writerGUID = WriterID} = HB,
                    #state{writer_proxies = WProxies} = State) ->
    Proxy = maps:get(WriterID, WProxies),
    NewProxy = manage_heartbeat_for_writer(HB, Proxy, State),
    State#state{writer_proxies =  maps:put(WriterID, NewProxy, WProxies)};
h_receive_heartbeat(#heartbeat_frag{writerGUID = WriterID} = HB,
                    #state{writer_proxies = WProxies} = State) ->
    Proxy = maps:get(WriterID, WProxies),
    NewProxy = manage_heartbeat_frag_for_writer(HB, Proxy, State),
    State#state{writer_proxies =  maps:put(WriterID, NewProxy, WProxies)}.

manage_heartbeat_for_writer(#heartbeat{writerGUID = WGUID,
                                       final_flag = FF,
                                       min_sn = Min,
                                       max_sn = Max},
                            #writer_proxy{changes = Changes} = W,
                            #state{heartbeatResponseDelay = Delay}) ->
    %io:format("~p\n",[WGUID]),
    NewChanges = missing_changes_update(Changes, Min, Max),
    NewChanges2 = lost_changes_update(NewChanges, Min, Max),
    erlang:send_after(Delay, self(), {send_acknack_if_needed, {WGUID, FF}}),
    W#writer_proxy{changes = NewChanges2}.

manage_heartbeat_frag_for_writer(#heartbeat_frag{writerGUID = WGUID,
                                                sequence_number = SN,
                                                last_fragment_number = LF,
                                                count = Count},
                                #writer_proxy{changes = Changes} = W,
                                #state{heartbeatResponseDelay = Delay}) ->
    ?LOG_NOTICE("Managing heartbeat frag ~p\n", [Count]),
    Change = maps:get(SN,
                  Changes,
                  #change_from_writer{fragmented = true,
                                      status = missing}),
    NewChanges = case Change#change_from_writer.fragmented of
        true ->
            % we use the heartbeatResponseDelay for nackfrags too
            erlang:send_after(Delay, self(), {send_nackfrag_if_needed, WGUID}),
            NewChange = Change#change_from_writer{last_available_fragment_number = LF},
            maps:put(SN, NewChange, Changes);
        false ->
            Changes
    end,
    W#writer_proxy{changes = NewChanges}.

missing_changes(Changes) ->
    maps:filter(fun
        (_, #change_from_writer{status = missing}) ->
            true;
        (_, _) ->
            false
    end, Changes).

fragmented_changes(Changes) ->
    maps:filter(fun
        (_, #change_from_writer{fragmented = true}) ->
            true;
        (_, _) ->
            false
    end, Changes).

available_change_max(Changes) when Changes == #{} ->
    0;
available_change_max(Changes) ->
    lists:max(maps:keys(Changes)).

% 0 means flag not set, an acknowledgment must be sent
h_send_acknack_if_needed(WGUID, 0, #state{writer_proxies = WP} = S) ->
    Proxy = maps:get(WGUID, WP),
    Missing = missing_changes(Proxy#writer_proxy.changes),
    Fragmented = fragmented_changes(Missing),
    MissingSN = case maps:size(Missing) of
        0 -> available_change_max(Proxy#writer_proxy.changes) + 1;
        _ -> maps:keys(Missing)
    end,
    S1 = send_acknack(WGUID, MissingSN, S),
    case maps:size(Fragmented) of
        0 -> S1;
        _ -> lists:foldl(fun(F, State) -> send_nackfrag(WGUID, F, State) end,
                        S1,
                        maps:to_list(Fragmented))
    end;
% 1 means flag set, i acknoledge only if i know to miss some data
h_send_acknack_if_needed(WGUID, 1, #state{writer_proxies = WP} = S) ->
    Proxy = maps:get(WGUID, WP),
    Missing = missing_changes(Proxy#writer_proxy.changes),
    Fragmented = fragmented_changes(Missing),
    case maps:size(Missing) of
        0 -> S;
        _ ->
            S1 = send_acknack(WGUID, maps:keys(Missing), S),
            case maps:size(Fragmented) of
                0 -> S1;
                _ -> lists:foldl(fun(F, State) -> send_nackfrag(WGUID, F, State) end,
                                S1,
                                maps:to_list(Fragmented))
            end
    end.

h_send_nackfrag_if_needed(_WGUID, #state{writer_proxies = _WP} = S) ->
    ?LOG_WARNING("Unimplemented nackfrag send"),
    S.

send_acknack(WGUID,
             Missing,
             #state{entity = #endPoint{guid = RGUID},
                    writer_proxies = WP,
                    acknack_count = C} =
                 S) ->
    %io:format("Sending an ack nack for ~p, to ~p\n", [Missing, WGUID#guId.entityId]),
    %io:format("Proxies are: ~p\n",[[ P || #writer_proxy{guid=G}=P <- WP, G == WGUID]]),
    Proxy = maps:get(WGUID, WP),
    U_List = Proxy#writer_proxy.unicastLocatorList,
    FirstL = hd(U_List),
    ACKNACK =
        #acknack{writerGUID = WGUID,
                 readerGUID = RGUID,
                 final_flag = 1,
                 sn_range = Missing,
                 count = C},
    Datagram =
        rtps_messages:build_message(RGUID#guId.prefix,
                                    [rtps_messages:serialize_info_dst(WGUID#guId.prefix),
                                     rtps_messages:serialize_acknack(ACKNACK)]),
    rtps_gateway:send(RGUID, Datagram, FirstL#locator.ip, FirstL#locator.port),
    S#state{acknack_count = C + 1}.

send_nackfrag(WGUID, {SN, Change}, #state{entity = #endPoint{guid = RGUID},
                                          history_cache = Cache,
                                          writer_proxies = WP,
                                          nackfrag_count = C} = S) ->
    Proxy = maps:get(WGUID, WP),
    #change_from_writer{
        expected_fragments = ExpFrag,
        last_available_fragment_number = LastAvailableFragmentNumber
    } = Change,
    CacheChange = rtps_history_cache:get_change(Cache, {WGUID, SN}),
    FragMap = case CacheChange of
        not_found -> #{};
        #cacheChange{data = Data} when is_map(Data) -> Data;
        F -> error({invalid_fragment_map, F})
    end,
    % If we never received a heartbeat, we use the expected number of fragments
    % otherwise we use the last available fragment number
    MaxFragNum = case LastAvailableFragmentNumber of
        undefined -> ExpFrag;
        LastFrag -> LastFrag
    end,
    % Ensure the range is valid
    % FragmentNumberSet are limited to belong
    % to an interval with a range no bigger than 256
    % maximum(FragmentNumberSet) - minimum(FragmentNumberSet) < 256
    % minimum(FragmentNumberSet) >= 1
    Available = lists:seq(1, MaxFragNum),
    Missing = lists:subtract(Available, maps:keys(FragMap)),
    Rangebase = lists:min(Missing),
    Max = lists:max(Missing),
    RangeMax = erlang:min(Max, Rangebase + 255),
    NackList = lists:filter(fun(N) -> N =< RangeMax end, Missing),
    % Stored = maps:keys(FragMap),
    % ?assert(ExpFrag - LF >= 1),
    % MaxRange = min(ExpFrag, LF + 255),
    Nackfrag = #nackfrag{writerGUID = WGUID,
                         readerGUID = RGUID,
                         sn = SN,
                         missing_fragments = NackList,
                         count = C},
    Datagram = rtps_messages:build_message(RGUID#guId.prefix,
                                         [rtps_messages:serialize_info_dst(WGUID#guId.prefix),
                                          rtps_messages:serialize_nackfrag(Nackfrag)]),
    U_List = Proxy#writer_proxy.unicastLocatorList,
    FirstL = hd(U_List),
    rtps_gateway:send(RGUID, Datagram, FirstL#locator.ip, FirstL#locator.port),
    S#state{nackfrag_count = C + 1}.

h_receive_data({WriterID, SN, Data},
               #state{history_cache = Cache, writer_proxies = WP} = State) ->
    Proxy = maps:get(WriterID, WP),
    rtps_history_cache:add_change(Cache, data_to_cache_change({WriterID, SN, Data})),
    Change = maps:get(SN,
                      Proxy#writer_proxy.changes,
                      #change_from_writer{status = missing}),
    ChangeReceived = Change#change_from_writer{status = received},
    NewChanges = maps:put(SN, ChangeReceived, Proxy#writer_proxy.changes),
    NewProxies = maps:put(WriterID, Proxy#writer_proxy{changes = NewChanges}, WP),
    State#state{writer_proxies = NewProxies}.

h_receive_data_frag(
            {WriterID, SN, #data_frag{sample_size = SampleSize} = DataFrag},
            #state{history_cache = Cache, writer_proxies = WP} = State) ->
    Proxy = maps:get(WriterID, WP),
    #writer_proxy{changes = Changes} = Proxy,
    ChangeFromWriter = maps:get(SN, Changes,
                                #change_from_writer{fragmented = true,
                                                    status = missing}),
    CurrentData =
        case rtps_history_cache:get_change(Cache, {WriterID, SN}) of
        not_found -> #{};
        Change -> Change#cacheChange.data
    end,
    case is_map(CurrentData) of
        true ->
            NewChange =
            case store_fragments(Cache, WriterID, SN, ChangeFromWriter, CurrentData, DataFrag) of
                C when C#change_from_writer.size_counter == SampleSize ->
                    ?LOG_NOTICE("Received all fragments"),
                    {DataSample, NewRec} = rebuild_sample(Cache, WriterID, SN, SampleSize, C),
                    store_sample(Cache, WriterID, SN, DataSample),
                    NewRec;
                C -> C
            end,
            NewProxy = Proxy#writer_proxy{
                changes = maps:put(SN, NewChange, Changes)
            },
            State#state{writer_proxies = maps:put(WriterID, NewProxy, WP)};
        false ->
            State
    end.


h_receive_gap(#gap{writerGUID = WriterID, sn_set = SET},
               #state{history_cache = _Cache, writer_proxies = WP} = State) ->
    Proxy = maps:get(WriterID, WP),
    %io:format("GAP processing...for numbers ~p\n", [SET]),
    %DEBUG_SN_STATES = [ {SN,S} || #change_from_writer{change_key = {_,SN}, status = S} <- Selected#writer_proxy.changes],
    %io:format("Current changes state ~p\n", [DEBUG_SN_STATES]),
    AddedChanges = lists:foldl(
        fun (SN, AllChanges) ->
            Change = maps:get(SN,
                              AllChanges,
                              #change_from_writer{status = missing}),
            maps:put(SN, Change, AllChanges)
        end,
        Proxy#writer_proxy.changes,
        SET),
    AddedAndMarked = maps:map(fun(SN, C) ->
                                case lists:member(SN, SET) of
                                    true -> C#change_from_writer{status = received};
                                    false -> C
                                end
                            end, AddedChanges),
    NewProxy = Proxy#writer_proxy{changes = AddedAndMarked},
    State#state{writer_proxies = maps:put(WriterID, NewProxy, WP)}.

store_fragments(Cache, WriterID, SN, ChangeFromWriter, CurrentData, DataFrag) ->
    #change_from_writer{size_counter = SizeCounter} = ChangeFromWriter,
    #data_frag{
        start_num = StartNum,
        count = Count,
        fragment_size = FragSize,
        sample_size = SampleSize,
        fragments = Fragments
     } = DataFrag,
    NumberedFrags = lists:zip(lists:seq(StartNum, StartNum + Count - 1),
                              split_fragments(Count, FragSize, Fragments)),

    FragsToStore = lists:filter(fun({N,_}) -> not maps:is_key(N, CurrentData) end,
                                NumberedFrags),
    NewFragsMap = maps:from_list(FragsToStore),
    rtps_history_cache:add_fragments(Cache, {WriterID, SN}, NewFragsMap),
    StoredSize = lists:foldl(fun
                ({_, B}, Size) -> Size + size(B)
            end,
            0,
            FragsToStore),
    NewSize = SizeCounter + StoredSize,
    ?assert(NewSize =< SampleSize),
    ChangeFromWriter#change_from_writer{
        fragmented = true,
        expected_fragments = SampleSize div FragSize + min(1, SampleSize rem FragSize),
        size_counter = NewSize
    }.

split_fragments(Count, Size, Payload) ->
    rec_split_fragments(Count, Size, Payload, []).

rec_split_fragments(1, _, LastFragment, Fragments) ->
    lists:reverse([LastFragment|Fragments]);
rec_split_fragments(Count, Size, Payload, Fragments) ->
    <<Frag:Size/binary, Rest/binary>> = Payload,
    rec_split_fragments(Count - 1, Size, Rest, [Frag|Fragments]).

rebuild_sample(Cache, WriterID, SN, SampleSize,
               #change_from_writer{fragmented = true} = ChangeFromWriter) ->
    Change = rtps_history_cache:get_change(Cache, {WriterID, SN}),
    Data = Change#cacheChange.data,
    <<EncapsulationKind:16,
      _:16, % unused encapsulationoptions
      SerializedPayload/binary>> = list_to_binary(
        [B || {_, B} <- lists:sort(maps:to_list(Data))]
    ),
    <<CDR_LE:16>> = ?CDR_LE,
    ?assertMatch(CDR_LE, EncapsulationKind),
    ?assert(size(SerializedPayload) == SampleSize - 4),
    NewRecord = ChangeFromWriter#change_from_writer{
        status = received,
        fragmented = false,
        size_counter = 0},
    {SerializedPayload, NewRecord}.

store_sample(Cache, WriterID, SN, Data) ->
    CacheChange = data_to_cache_change({WriterID, SN, Data}),
    rtps_history_cache:add_change(Cache, CacheChange).

message_matches_proxy(#heartbeat{writerGUID = WriterID},
                        #state{writer_proxies = WP}) ->
    maps:is_key(WriterID, WP);
message_matches_proxy(#heartbeat_frag{writerGUID = WriterID},
                        #state{writer_proxies = WP}) ->
    maps:is_key(WriterID, WP);
message_matches_proxy(#gap{writerGUID = WriterID},
                        #state{writer_proxies = WP}) ->
    maps:is_key(WriterID, WP);
message_matches_proxy(#guId{} = WriterID, #state{writer_proxies = WP}) ->
    maps:is_key(WriterID, WP);
message_matches_proxy({#guId{} = WriterID, _, _}, #state{writer_proxies = WP}) ->
    maps:is_key(WriterID, WP).
