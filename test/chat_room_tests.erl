-module(chat_room_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("chat_room_msg.hrl").
-include("chat_room_user_state.hrl").

make_user_test() ->
    ?assertEqual(
        #user{
            name = <<"user">>,
            pid = "<0.0.0>"
        },
        chat_room:make_user(
            #user_enter_to_room{
                user = <<"user">>,
                from = "<0.0.0>"
            }
        )
    ).

make_user_enter_to_room_notify_test() ->
    ?assertMatch(
        #user_enter_to_room_notify{
            user_name = <<"user">>,
            user_pid = "<0.0.0>"
        },
        chat_room:make_user_enter_to_room_notify(
            #user{
                name = <<"user">>,
                pid = "<0.0.0>"
            }
        )
    ).

make_user_leave_from_room_notify_test() ->
    ?assertMatch(
        #user_leave_from_room_notify{
            user_name = <<"user">>,
            user_pid = "<0.0.0>"
        },
        chat_room:make_user_leave_from_room_notify(
            #user{
                name = <<"user">>,
                pid = "<0.0.0>"
            }
        )
    ).

make_user_msg_to_room_notify_test() ->
    ?assertMatch(
        #user_msg_to_room_notify{
            user_name = <<"user name">>,
            user_pid = "<0.0.0>",
            msg_body = <<"Message body">>
        },
        chat_room:make_user_msg_to_room_notify(
            #user_msg_to_room{
                body = <<"Message body">>
            },
            #user{
                name = <<"user name">>,
                pid = "<0.0.0>"
            }
        )
    ).

-endif.
