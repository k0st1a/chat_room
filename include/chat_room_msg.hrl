%% Запрос пользователя о входе в команту
-record(user_enter_to_room, {
    user :: binary(),
    from :: pid()
}).
-type user_enter_to_room() :: #user_enter_to_room{}.

%% Уведомление о входе пользователя в комнату
-record(user_enter_to_room_notify, {
    user_name :: binary(),
    user_pid :: binary(),
    timestamp = erlang:timestamp()
}).
-type user_enter_to_room_notify() :: #user_enter_to_room_notify{}.

%% Уведомление о выходе пользователя из комнаты
-record(user_leave_from_room_notify, {
    user_name :: binary(),
    user_pid :: binary(),
    timestamp = erlang:timestamp()
}).
-type user_leave_from_room_notify() :: #user_leave_from_room_notify{}.
