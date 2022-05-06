-module(rtps_network_utils).

-export([
    wait_dhcp/1,
    get_local_ip/0
]).

% API -----------------------------------------------------------------
%
wait_dhcp(Millis) ->
    Self = self(),
    Pid = spawn(fun() -> do_wait_dhcp(Self) end),
    receive
        ip_available -> ip_available
    after Millis -> exit(Pid, timeout), timeout
    end.

get_local_ip() ->
    get_ip_of_valid_interfaces(no_loopback).

% INTERNAL -----------------------------------------------------------------

do_wait_dhcp(Pid) ->
    case get_local_ip() of
        {_,_,_,_}=IP when IP /= {127,0,0,1} -> Pid ! ip_available;
        _ -> timer:sleep(100), do_wait_dhcp(Pid)
    end.

get_ipv4_from_opts([]) ->
    undefined;
get_ipv4_from_opts([{addr, {_1, _2, _3, _4}} | _]) ->
    {_1, _2, _3, _4};
get_ipv4_from_opts([_ | TL]) ->
    get_ipv4_from_opts(TL).

has_ipv4(Opts) ->
    get_ipv4_from_opts(Opts) =/= undefined.

flags_are_ok(Flags, allow_looback) ->
    lists:member(up, Flags) and
        lists:member(running, Flags);
flags_are_ok(Flags, no_loopback) ->
    lists:member(up, Flags) and
        lists:member(running, Flags) and
        not lists:member(loopback, Flags).

get_valid_interfaces(CanUseLoopback) ->
    {ok, Interfaces} = inet:getifaddrs(),
    [
        Opts
     || {_Name, [{flags, Flags} | Opts]} <- Interfaces,
        flags_are_ok(Flags, CanUseLoopback),
        has_ipv4(Opts)
    ].

get_ip_of_valid_interfaces(CanUseLoopback) ->
    case get_valid_interfaces(CanUseLoopback) of
        [Opts | _] -> get_ipv4_from_opts(Opts);
        _ when CanUseLoopback == no_loopback -> get_ip_of_valid_interfaces(allow_looback);
        _ -> undefined
    end.

