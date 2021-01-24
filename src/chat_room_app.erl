-module(chat_room_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    lager:info("Start", []),
    chat_room_sup:start_link().

stop(_State) ->
    lager:info("Stop", []),
    ok.
