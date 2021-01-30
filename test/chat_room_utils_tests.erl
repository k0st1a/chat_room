-module(chat_room_utils_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

to_binary_test_() -> [
    ?_assertEqual(
        <<"binary">>,
        chat_room_utils:to_binary(<<"binary">>)
    ),
    ?_assertEqual(
        <<"list">>,
        chat_room_utils:to_binary(<<"list">>)
    ),
    ?_assertEqual(
        <<"<0.0.0>">>,
        chat_room_utils:to_binary(list_to_pid("<0.0.0>"))
    )
].

-endif.
