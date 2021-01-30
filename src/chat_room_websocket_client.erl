-module(chat_room_websocket_client).

-include("msg.hrl").
-include("chat_room_msg.hrl").

-export([
    %% cowboy handler callbacks
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2
]).

-record(state, {
    room_pid :: pid()
}).

init(Req, State) ->
    lager:debug("Init, State:~1000000p, Req:~n~p", [State, Req]),
    {cowboy_websocket, Req, State, #{max_frame_size => 8000000}}.

websocket_init(State) ->
    lager:debug("Init, State:~n~p", [State]),
    {ok, RoomPid} = chat_room_manager:start_room(),
    lager:debug("RoomPid:~1000p", [RoomPid]),
    Text = make_websocket_init_resp(self()),
    {[{text, Text}], #state{room_pid = RoomPid}}.
%
websocket_handle({text, Text}, #state{room_pid = Pid} = State) ->
    lager:debug("handle, Text:~p", [Text]),
    case chat_room_msg_coder:decode(Text) of
        {ok, Decoded} ->
            lager:debug("Pid:~1000p, Decoded:~n~p", [Pid, Decoded]),
            Msg = chat_room_utils:make_msg(self(), Decoded),
            lager:debug("Msg:~n~p", [Msg]),
            chat_room:cast(Pid, Msg);
        _Error ->
            lager:debug("Error:~p", [_Error])
    end,
    {ok, State};
websocket_handle(_Frame, State) ->
    lager:debug("handle, Unknown Frame:~n~p", [_Frame]),
    {ok, State}.

websocket_info(#msg{} = Info, State) ->
    lager:debug("info, Info:~p", [Info]),
    Frame = encode_and_frame(Info#msg.body),
    lager:debug("Frame:~p", [Frame]),
    {Frame, State};
websocket_info(_Info, State) ->
    lager:debug("info, Unknown Info:~n~p", [_Info]),
    {ok, State}.

-spec encode_and_frame(Msg :: term()) -> [{text, binary()}] | ok.
encode_and_frame(Msg) ->
    case chat_room_msg_coder:encode(Msg) of
        {ok, Encoded} ->
            [{text, Encoded}];
        _ ->
            ok
    end.

-spec make_websocket_init_resp(Pid :: pid()) -> binary().
make_websocket_init_resp(Pid) ->
    <<"{\"websocket_init_resp\":{\"pid\":\"", (chat_room_utils:to_binary(Pid))/binary, "\"}}">>.
