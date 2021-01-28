-record(start_room_bot, {
    bot_name :: binary(),
    room_pid :: pid()
}).
-type start_room_bot() :: #start_room_bot{}.

-record(stop_room_bot, {
}).
-type stop_room_bot() :: #stop_room_bot{}.
