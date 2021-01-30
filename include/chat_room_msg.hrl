%% Запрос пользователя о входе в команту
-record(user_enter_to_room, {
    user :: binary(),
    is_bot = false :: boolean()
}).
-type user_enter_to_room() :: #user_enter_to_room{}.

%% Уведомление о входе пользователя в комнату
-record(user_enter_to_room_notify, {
    user_name :: binary(),
    user_pid :: binary()
}).
-type user_enter_to_room_notify() :: #user_enter_to_room_notify{}.

%% Уведомление о выходе пользователя из комнаты
-record(user_leave_from_room_notify, {
    user_name :: binary(),
    user_pid :: binary()
}).
-type user_leave_from_room_notify() :: #user_leave_from_room_notify{}.

%% Сообщение от пользователя в комнату
-record(user_msg_to_room, {
    body :: term(),
    from :: pid()
}).
-type user_msg_to_room() :: #user_msg_to_room{}.

%% Уведомление о сообщении от пользователя в комнату
-record(user_msg_to_room_notify, {
    user_name :: binary(),
    user_pid :: pid(),
    msg_body :: term()
}).
-type user_msg_to_room_notify() :: #user_msg_to_room_notify{}.

-type chat_room_msg_to_room() :: user_enter_to_room()
                               | user_msg_to_room().

-type chat_room_msg_from_room() :: user_enter_to_room_notify()
                                 | user_leave_from_room_notify()
                                 | user_msg_to_room_notify().
