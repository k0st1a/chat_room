-module(chat_room_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include("msg.hrl").
-include("chat_room_msg.hrl").
-include("chat_room_user_state.hrl").

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
    user_enter_to_room/1,
    user_leave_from_room/1,
    user_msg_to_room/1
]).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() -> [
    user_enter_to_room,
    user_leave_from_room,
    user_msg_to_room
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
    Config.

end_per_testcase(_TestCase, _Config) ->
    chat_room_api:stop(),
    chat_room_api:nolager(),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================
user_enter_to_room(_Config) ->
    lager:debug("Start room", []),
    {ok, Pid} = chat_room_manager:start_room(),

    lager:debug("Send user_enter_to_room", []),
    chat_room:cast(
        Pid,
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
    end.

user_leave_from_room(_Config) ->
    lager:debug("Start room", []),
    {ok, Pid} = chat_room_manager:start_room(),

    lager:debug("Send user_enter_to_room", []),
    chat_room:cast(
        Pid,
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

    lager:debug("Start second user to room", []),
    %% запускаем второго пользователя в комнату
    Pid2 =
        erlang:spawn(
            fun () ->
                    chat_room:cast(
                        Pid,
                        #msg{
                            from = self(),
                            body = #user_enter_to_room{user = <<"user name 2">>}
                        }
                    )
            end
        ),

    lager:debug("Wait user_enter_to_room_notify by second user", []),
    %% ожидаем нотификацию о входе второго пользователя в комнату
    receive
        #msg{body = #user_enter_to_room_notify{} = Body2} ->
            ?assertEqual(
                <<"user name 2">>,
                Body2#user_enter_to_room_notify.user_name
            ),
            ?assertEqual(
                Pid2,
                Body2#user_enter_to_room_notify.user_pid
            )
    end,

    lager:debug("Wait user_leave_from_room_notify by second user", []),
    %% ожидаем нотификацию о выходе второго пользователя из комнаты
    receive
        #msg{body = #user_leave_from_room_notify{} = Body3} ->
            ?assertEqual(
                <<"user name 2">>,
                Body3#user_leave_from_room_notify.user_name
            ),
            ?assertEqual(
                Pid2,
                Body3#user_leave_from_room_notify.user_pid
            )
    end.

user_msg_to_room(_Config) ->
    lager:debug("Start room", []),
    {ok, Pid} = chat_room_manager:start_room(),

    lager:debug("Send user_enter_to_room", []),
    chat_room:cast(
        Pid,
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

    lager:debug("Send user_msg_to_room", []),
    chat_room:cast(
        Pid,
        #msg{
            from = self(),
            body = #user_msg_to_room{body = <<"My message">>}
        }
    ),

    lager:debug("Wait user_msg_to_room_notify", []),
    %% ожидаем нотификацию о сообщении от пользователя в комнату
    receive
        #msg{body = #user_msg_to_room_notify{} = Body2} ->
            ?assertEqual(
                <<"user name">>,
                Body2#user_msg_to_room_notify.user_name
            ),
            ?assertEqual(
                self(),
                Body2#user_msg_to_room_notify.user_pid
            ),
            ?assertEqual(
                <<"My message">>,
                Body2#user_msg_to_room_notify.msg_body
            )
    end.
