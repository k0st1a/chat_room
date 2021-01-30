-module(chat_room_bot_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include("msg.hrl").
-include("chat_room_msg.hrl").
-include("chat_room_user_state.hrl").
-include("chat_room_bot_msg.hrl").

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
    check_room_bot/1
]).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() -> [
    check_room_bot
].

suite() ->
    [{timetrap, {seconds, 3}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    chat_room_api:lager(),
    chat_room_api:start(),
    meck:new(chat_room_bot, [passthrough]),
    meck:expect(chat_room_bot, generate_timeout, fun () -> 100 end),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(chat_room_bot),
    chat_room_api:stop(),
    chat_room_api:nolager(),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================
check_room_bot(_Config) ->
    lager:debug("Start room", []),
    {ok, RoomPid} = chat_room_manager:start_room(),

    lager:debug("Send user_enter_to_room", []),
    chat_room:cast(
        RoomPid,
        #msg{
            from = self(),
            body = #user_enter_to_room{user = <<"user name">>}
        }
    ),

    lager:debug("Wait user_enter_to_room_notify", []),
    %% ожидаем нотификацию о входе пользователя в комнату
    receive
        #msg{body = #user_enter_to_room_notify{} = Body} ->
            ?assertEqual(
                <<"user name">>,
                Body#user_enter_to_room_notify.user_name
            ),
            ?assertEqual(
                self(),
                Body#user_enter_to_room_notify.user_pid
            )
    end,

    lager:debug("Start bot", []),
    %% запускаем бота в комнату
    {ok, BotPid} = chat_room:call(RoomPid, #start_room_bot{bot_name = <<"bot name">>, room_pid = RoomPid}),

    lager:debug("Wait user_enter_to_room_notify by boot", []),
    %% ловим нотификацию о входе бота в комнату
    receive
        #msg{body = #user_enter_to_room_notify{} = Body2} ->
            ?assertEqual(
                <<"bot name">>,
                Body2#user_enter_to_room_notify.user_name
            ),
            ?assertEqual(
                BotPid,
                Body2#user_enter_to_room_notify.user_pid
            )
    end,

    lager:debug("Wait user_msg_to_room_notify by boot", []),
    %% ловим нотификацию о сообщении от бота в комнате
    receive
        #msg{body = #user_msg_to_room_notify{} = Body3} ->
            ?assertEqual(
                <<"bot name">>,
                Body3#user_msg_to_room_notify.user_name
            ),
            ?assertEqual(
                BotPid,
                Body3#user_msg_to_room_notify.user_pid
            ),
            ?assertEqual(
                <<"Message from bot">>,
                Body3#user_msg_to_room_notify.msg_body
            )
    end,

    lager:debug("Stop bot", []),
    %% Останавливаем бота
    ok = chat_room:call(RoomPid, #stop_room_bot{}),

    lager:debug("Wait user_leave_from_room_notify by boot", []),
    %% ловим нотификацию о выходе бота из комнаты
    receive
        #msg{body = #user_leave_from_room_notify{} = Body4} ->
            ?assertEqual(
                <<"bot name">>,
                Body4#user_leave_from_room_notify.user_name
            ),
            ?assertEqual(
                BotPid,
                Body4#user_leave_from_room_notify.user_pid
            )
    end.
