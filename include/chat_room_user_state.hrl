-type user_options() :: #{
    bot => true
}.
-record(user, {
    name :: binary(),
    pid :: pid(),
    ref :: reference(),
    options = #{} :: user_options()
}).
-type user() :: #user{}.
-type users() :: [user()].
