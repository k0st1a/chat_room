-record(msg, {
    from :: pid(),
    body :: term()
}).
-type msg() :: #msg{}.
