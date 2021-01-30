-module(chat_room_app).

-behaviour(application).

-export([
    start/2,
    prep_stop/1,
    stop/1
]).

start(_Type, _Args) ->
    lager:info("Start", []),
	Dispatch =
        cowboy_router:compile([
            {'_', [
                {"/", cowboy_static, {priv_file, chat_room, "index.html"}},
                {"/websocket", chat_room_websocket_client, []}
            ]}
        ]),
    TransportOptions = [
        {port, application:get_env(chat_room, port, 8080)}
    ],
    ProtocolOptions = #{env => #{dispatch => Dispatch}},
    case cowboy:start_clear(chat_room_http_listener, TransportOptions, ProtocolOptions) of
        {ok, _} ->
            chat_room_sup:start_link();
        {error, _} = Error ->
            Error
    end.

prep_stop(State) ->
    lager:info("Prep stop", []),
    ok = ranch:suspend_listener(chat_room_http_listener),
    ok = ranch:wait_for_connections(chat_room_http_listener, '==', 0),
    ok = ranch:stop_listener(chat_room_http_listener),
    State.

stop(_State) ->
    lager:info("Stop", []),
    ok.
