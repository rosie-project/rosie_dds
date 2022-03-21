-module(rtps_network_utils).

-export([get_local_ip/0]).

get_ipv4_from_opts([]) ->
    undefined;
get_ipv4_from_opts([{addr, {_1, _2, _3, _4}} | _]) ->
    {_1, _2, _3, _4};
get_ipv4_from_opts([_ | TL]) ->
    get_ipv4_from_opts(TL).

has_ipv4(Opts) ->
    get_ipv4_from_opts(Opts) =/= undefined.

flags_are_ok(Flags, true) ->
    lists:member(up, Flags) and
        lists:member(running, Flags);
flags_are_ok(Flags, false) ->
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
        _ when CanUseLoopback == false -> get_ip_of_valid_interfaces(true);
        _ -> undefined
    end.

get_local_ip() -> get_ip_of_valid_interfaces(false).
