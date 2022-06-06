-module(rosie_dds_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dds_sup:start_link().

stop(_State) ->
    ok.
