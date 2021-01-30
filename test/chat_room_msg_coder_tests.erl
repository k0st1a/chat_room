-module(chat_room_msg_coder_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("chat_room_msg.hrl").

compose_test_() -> [
    {"check compose #user_enter_to_room_notify{}", fun compose_user_enter_to_room_notify/0},
    {"check compose #user_leave_from_room_notify{}", fun compose_user_leave_from_room_notify/0},
    {"check compose #user_msg_to_room_notify{}", fun compose_user_msg_to_room_notify/0}
].

encode_test_() -> [
    {"check encode #user_enter_to_room_notify{}", fun encode_user_enter_to_room_notify/0},
    {"check encode #user_leave_from_room_notify{}", fun encode_user_leave_from_room_notify/0},
    {"check encode #user_msg_to_room_notify{}", fun encode_user_msg_to_room_notify/0}
].

parse_test_() -> [
    {"check parse user_enter_to_room", fun parse_user_enter_to_room/0},
    {"check parse user_msg_to_room", fun parse_user_msg_to_room/0}
].

decode_test_() -> [
    {"check decode user_enter_to_room", fun decode_user_enter_to_room/0},
    {"check decode user_msg_to_room", fun decode_user_msg_to_room/0}
].

compose_user_enter_to_room_notify() ->
    ?assertEqual(
        #{
            <<"user_enter_to_room_notify">> => #{
                <<"user_name">> => <<"UserName">>,
                <<"user_pid">> => <<"<0.0.0>">>
            }
        },
        chat_room_msg_coder:compose(
            #user_enter_to_room_notify{
                user_name = <<"UserName">>,
                user_pid = list_to_pid("<0.0.0>")
            }
        )
    ).

compose_user_leave_from_room_notify() ->
    ?assertEqual(
        #{
            <<"user_leave_from_room_notify">> => #{
                <<"user_name">> => <<"UserName">>,
                <<"user_pid">> => <<"<0.0.0>">>
            }
        },
        chat_room_msg_coder:compose(
            #user_leave_from_room_notify{
                user_name = <<"UserName">>,
                user_pid = list_to_pid("<0.0.0>")
            }
        )
    ).

compose_user_msg_to_room_notify() ->
    ?assertEqual(
        #{
            <<"user_msg_to_room_notify">> => #{
                <<"user_name">> => <<"UserName">>,
                <<"user_pid">> => <<"<0.0.0>">>,
                <<"msg_body">> => <<"MsgBody">>
            }
        },
        chat_room_msg_coder:compose(
            #user_msg_to_room_notify{
                user_name = <<"UserName">>,
                user_pid = list_to_pid("<0.0.0>"),
                msg_body = <<"MsgBody">>
            }
        )
    ).

encode_user_enter_to_room_notify() ->
    ?assertEqual(
        {ok, <<"{\"user_enter_to_room_notify\":{\"user_name\":\"UserName\",\"user_pid\":\"<0.0.0>\"}}">>},
        chat_room_msg_coder:encode(
            #user_enter_to_room_notify{
                user_name = <<"UserName">>,
                user_pid = list_to_pid("<0.0.0>")
            }
        )
    ).

encode_user_leave_from_room_notify() ->
    ?assertEqual(
        {ok, <<"{\"user_leave_from_room_notify\":{\"user_name\":\"UserName\",\"user_pid\":\"<0.0.0>\"}}">>},
        chat_room_msg_coder:encode(
            #user_leave_from_room_notify{
                user_name = <<"UserName">>,
                user_pid = list_to_pid("<0.0.0>")
            }
        )
    ).

encode_user_msg_to_room_notify() ->
    ?assertEqual(
        {ok, <<"{\"user_msg_to_room_notify\":{\"msg_body\":\"MsgBody\",\"user_name\":\"UserName\",\"user_pid\":\"<0.0.0>\"}}">>},
        chat_room_msg_coder:encode(
            #user_msg_to_room_notify{
                user_name = <<"UserName">>,
                user_pid = list_to_pid("<0.0.0>"),
                msg_body = <<"MsgBody">>
            }
        )
    ).

parse_user_enter_to_room() ->
    ?assertEqual(
        #user_enter_to_room{
            user = <<"UserName">>
        },
        chat_room_msg_coder:parse(
            #{
                <<"user_enter_to_room">> => #{
                    <<"user">> => <<"UserName">>
                }
            }
        )
    ).

parse_user_msg_to_room() ->
    ?assertEqual(
        #user_msg_to_room{
            body = <<"MsgBody">>
        },
        chat_room_msg_coder:parse(
            #{
                <<"user_msg_to_room">> => #{
                    <<"body">> => <<"MsgBody">>
                }
            }
        )
    ).

decode_user_enter_to_room() ->
    JSON = <<"{\"user_enter_to_room\":{\"user\":\"UserName\"}}">>,
    {ok, Decoded} = chat_room_msg_coder:decode(JSON),
    ?assertEqual(
        #user_enter_to_room{
            user = <<"UserName">>
        },
        Decoded
    ).

decode_user_msg_to_room() ->
    JSON = <<"{\"user_msg_to_room\":{\"body\":\"MsgBody\"}}">>,
    {ok, Decoded} = chat_room_msg_coder:decode(JSON),
    ?assertEqual(
        #user_msg_to_room{
            body = <<"MsgBody">>
        },
        Decoded
    ).

-endif.
