-record(user, {
    name :: binary(),
    pid :: pid(),
    ref :: reference()
}).
-type user() :: #user{}.
-type users() :: [user()].
