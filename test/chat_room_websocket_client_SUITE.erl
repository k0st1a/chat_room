-module(chat_room_websocket_client_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include("chat_room_msg.hrl").
-include("msg.hrl").

%% API
-export([
    all/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    check_websocket_client/1
]).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() -> [
    check_websocket_client
].

suite() ->
    [{timetrap, {seconds, 6}}].

init_per_suite(Config) ->
    [
        {host, application:get_env(chat_room, host, "localhost")},
        {port, application:get_env(chat_room, port, 8080)}
    ] ++
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    chat_room_api:lager(),
    chat_room_api:start(),
    chat_room_api:gun(),
    meck:new(chat_room_manager, [passthrough]),
    meck:expect(
        chat_room_manager,
        start_room,
        fun (_) -> chat_room_manager:start_room() end
    ),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(chat_room_manager),
    chat_room_api:stop(),
    chat_room_api:nolager(),
    chat_room_api:nogun(),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================
check_websocket_client(Config) ->
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),
    {ok, ConnPid} = gun:open(Host, Port),
    {ok, http} = gun:await_up(ConnPid),
    StreamRef = gun:ws_upgrade(ConnPid, "/websocket"),

    %% проверяем, что успешно переключились на websocket
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            ok;
        {gun_response, ConnPid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, ConnPid, StreamRef, Reason} ->
            exit({ws_upgrade_failed, Reason})
    after 1000 ->
        exit(timeout)
    end,

    lager:debug("Wait websocket_init_resp", []),
    %% ожидаем pid удаленного процесса
    receive
        {gun_ws, ConnPid, StreamRef, {text, Text}} ->
            WebsocketInitResp = jsx:decode(Text),
            #{
                <<"websocket_init_resp">> := #{
                    <<"pid">> := RemotePid
                }
            } = WebsocketInitResp
    end,

    lager:debug("Send user_enter_to_room", []),
    %% отправляем сообщение о входе пользователя(меня) в комнату
    gun:ws_send(
        ConnPid,
        {text, <<"{\"user_enter_to_room\":{\"user\":\"UserName\"}}">>}
    ),

    lager:debug("Wait user_enter_to_room_notify", []),
    %% ожидаем сообщение о входе пользователя(меня) в комнату
    receive
        {gun_ws, ConnPid, StreamRef, {text, Text2}} ->
            ?assertEqual(
                <<"{\"user_enter_to_room_notify\":{\"user_name\":\"UserName\",\"user_pid\":\"", RemotePid/binary ,"\"}}">>,
                Text2
            )
    end,

    lager:debug("Send user_msg_to_room", []),
    %% отправляем сообщение в комнату
    gun:ws_send(
        ConnPid,
        {text, <<"{\"user_msg_to_room\":{\"body\":\"MySomeMsg\"}}">>}
    ),

    lager:debug("Wait user_msg_to_room", []),
    %% ожидаем нотификацию о сообщении от пользователя(меня) в комнату
    receive
        {gun_ws, ConnPid, StreamRef, {text, Text3}} ->
            ?assertEqual(
                <<"{\"user_msg_to_room_notify\":{\"msg_body\":\"MySomeMsg\",\"user_name\":\"UserName\",\"user_pid\":\"", RemotePid/binary ,"\"}}">>,
                Text3
            )
    end,

    lager:debug("Close connection", []),
    %% закрываем соединение
    gun:close(ConnPid).
