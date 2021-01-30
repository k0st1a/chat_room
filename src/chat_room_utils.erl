-module(chat_room_utils).

-export([
    make_msg/2,
    send/2,
    to_binary/1
]).

-include("msg.hrl").

-spec make_msg(From :: pid(), Body :: term()) -> msg().
make_msg(From, Body) ->
    #msg{from = From, body = Body}.

-spec send(To :: pid(), Msg :: term()) -> ok.
send(To, Msg) ->
    erlang:send(To, Msg),
    ok.

-spec to_binary(Value :: term()) -> binary().
to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value)   -> to_binary(list_to_binary(Value));
to_binary(Value) when is_pid(Value)    -> to_binary(pid_to_list(Value));
to_binary(_)                           -> <<>>.
