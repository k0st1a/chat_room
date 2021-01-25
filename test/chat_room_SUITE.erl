-module(chat_room_SUITE).

-include_lib("eunit/include/eunit.hrl").
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
    user_leave_from_room/1
]).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() -> [
    user_enter_to_room,
    user_leave_from_room
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
    {ok, Pid} = chat_room_manager:start_room(),
    chat_room:cast(
        Pid,
        #user_enter_to_room{user = <<"user name">>, from = self()}
    ),
    %% ловим нотификацию о входе пользователя в комнату
    receive
        #user_enter_to_room_notify{} = Body->
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
    {ok, Pid} = chat_room_manager:start_room(),
    chat_room:cast(
        Pid,
        #user_enter_to_room{user = <<"user name">>, from = self()}
    ),
    %% ловим нотификацию о входе пользователя в комнату
    receive
        #user_enter_to_room_notify{} = Body->
            ?assertEqual(
                <<"user name">>,
                Body#user_enter_to_room_notify.user_name
            ),
            ?assertEqual(
                self(),
                Body#user_enter_to_room_notify.user_pid
            )
    end,
    %% запускаем второго пользователя в комнату
    Pid2 =
        erlang:spawn(
            fun () ->
                    chat_room:cast(
                        Pid,
                        #user_enter_to_room{user = <<"user name 2">>, from = self()}
                    )
            end
        ),
    %% ловим нотификацию о входе второго пользователя в комнату
    receive
        #user_enter_to_room_notify{} = Body2 ->
            ?assertEqual(
                <<"user name 2">>,
                Body2#user_enter_to_room_notify.user_name
            ),
            ?assertEqual(
                Pid2,
                Body2#user_enter_to_room_notify.user_pid
            )
    end,
    %% ловим нотификацию о выходе второго пользователя из комнаты
    receive
        #user_leave_from_room_notify{} = Body3->
            ?assertEqual(
                <<"user name 2">>,
                Body3#user_leave_from_room_notify.user_name
            ),
            ?assertEqual(
                Pid2,
                Body3#user_leave_from_room_notify.user_pid
            )
    end.
