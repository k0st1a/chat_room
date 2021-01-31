-module(chat_room_msg_coder).

-include("chat_room_msg.hrl").

-export([
    encode/1,
    decode/1
]).

-ifdef(TEST).
-compile(export_all).
-endif.

-spec encode(Msg :: chat_room_msg_from_room()) -> {ok, binary()} | error.
encode(Msg) ->
    try
        Compsed = compose(Msg),
        Encoded = jsx:encode(Compsed),
        {ok, Encoded}
    catch
        T:E:ST ->
           %io:format(
           %    user,
           %    "Runtime error of encode, T:~1000p, E:~1000p"
           %    "~nST:~n ~p"
           %    "~nMsg:~n ~p~n",
           %    [T, E, ST, Msg]
           %),
            lager:warning(
                "Runtime error of encode, T:~1000p, E:~1000p"
                "~nST:~n ~p"
                "~nMsg:~n ~p",
                [T, E, ST, Msg]
            ),
            error
    end.

-spec compose(Msg :: chat_room_msg_from_room()) -> map().
compose(#user_enter_to_room_notify{} = Msg) ->
    #{
        <<"user_enter_to_room_notify">> =>
            #{
                <<"user_name">> =>
                    chat_room_utils:to_binary(Msg#user_enter_to_room_notify.user_name),
                <<"user_pid">> =>
                    chat_room_utils:to_binary(Msg#user_enter_to_room_notify.user_pid)
            }
    };
compose(#user_leave_from_room_notify{} = Msg) ->
    #{
        <<"user_leave_from_room_notify">> =>
            #{
                <<"user_name">> =>
                    chat_room_utils:to_binary(Msg#user_leave_from_room_notify.user_name),
                <<"user_pid">> =>
                    chat_room_utils:to_binary(Msg#user_leave_from_room_notify.user_pid)
            }
    };
compose(#user_msg_to_room_notify{} = Msg) ->
    #{
        <<"user_msg_to_room_notify">> =>
            #{
                <<"user_name">> =>
                    chat_room_utils:to_binary(Msg#user_msg_to_room_notify.user_name),
                <<"user_pid">> =>
                    chat_room_utils:to_binary(Msg#user_msg_to_room_notify.user_pid),
                <<"msg_body">> =>
                    chat_room_utils:to_binary(Msg#user_msg_to_room_notify.msg_body)
            }
    }.

-spec decode(JSON :: binary()) -> {ok, term()} | error.
decode(JSON) ->
    try
        Decoded = jsx:decode(JSON, [return_maps]),
        Parsed = parse(Decoded),
        {ok, Parsed}
    catch
        T:E:ST ->
           %io:format(
           %    user,
           %    "Runtime error of decode, T:~1000p, E:~1000p"
           %    "~nST:~n ~p"
           %    "~nJSON:~n ~p",
           %    [T, E, ST, JSON]
           %),
            lager:warning(
                "Runtime error of decode, T:~1000p, E:~1000p"
                "~nST:~n ~p"
                "~nJSON:~n ~p",
                [T, E, ST, JSON]
            ),
            error
    end.

-spec parse(Decoded :: map()) -> chat_room_msg_to_room().
parse(#{<<"user_enter_to_room">> := Value}) ->
    #user_enter_to_room{
        user = maps:get(<<"user">>, Value)
    };
parse(#{<<"user_msg_to_room">> := Value}) ->
    #user_msg_to_room{
        body = maps:get(<<"body">>, Value)
    }.
