-module(chat_room_bot).

-behaviour(gen_server).

-include("msg.hrl").
-include("chat_room_msg.hrl").
-include("chat_room_bot_msg.hrl").

-export([
    %% API
    start_link/1,
    generate_timeout/0, %% for mocking this method
    %% gen_server callbacks
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-ifdef(TEST).
-compile(export_all).
-endif.

-record(state, {
    room_pid :: undefined | pid(),
    room_ref :: undefined | reference(),
    timer_ref :: undefined | reference()
}).

-define(TIMER_SEND_MSG, timer_send_msg).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Args :: term()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(#start_room_bot{room_pid = RoomPid, bot_name = BotName}) ->
    lager:info("Init, RoomPid:~1000p, BotName:~1000p", [RoomPid, BotName]),
    RoomRef = monitor(process, RoomPid),
    Timeout = chat_room_bot:generate_timeout(),
    TimerRef = start_timer(?TIMER_SEND_MSG, Timeout),
    lager:info("RoomRef:~1000p, TimerRef:~1000p, Timeout:~1000p", [RoomRef, TimerRef, Timeout]),
    Body =
        #user_enter_to_room{
            user = BotName,
            is_bot = true
        },
    Msg = chat_room_utils:make_msg(self(), Body),
    chat_room:cast(RoomPid, Msg),
    {ok, #state{room_pid = RoomPid, room_ref = RoomRef, timer_ref = TimerRef}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Msg, _From, State) ->
    lager:debug("Unknown handle_call, From: ~100p, Msg:~p", [_From, _Msg]),
    {reply, {error, unknown_msg}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    lager:debug("Unknown handle_cast, Msg:~p", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% room
handle_info({'DOWN', Ref, process, Pid, Reason}, #state{room_pid = Pid} = State) ->
    lager:debug("Room is down, Pid:~1000p, Ref:~1000p, Reason:~1000p", [Pid, Ref, Reason]),
    {stop, normal, State};
%% timer
handle_info({timeout, Ref, Id}, #state{timer_ref = Ref} = State) ->
    lager:debug("Timer fired, Ref:~100p, Id:~1000p", [Ref, Id]),
    send_some_msg_to_room(State#state.room_pid),
    Timeout = chat_room_bot:generate_timeout(),
    TimerRef = start_timer(?TIMER_SEND_MSG, Timeout),
    lager:debug("TimerRef:~1000p", [TimerRef]),
    {noreply, State#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    lager:debug("Skip handle_info, Info:~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:info("Terminate, Reason: ~p", [_Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec generate_timeout() -> non_neg_integer().
generate_timeout() ->
    rand:uniform(10000). %% 1 =< X =< N

-spec start_timer(Id :: atom(), Timeout :: non_neg_integer()) -> Ref :: reference().
start_timer(Id, Timeout) when erlang:is_integer(Timeout) andalso (Timeout >= 0) ->
    Ref = erlang:start_timer(Timeout, self(), Id),
    lager:debug("Start timer, Id:~100p, Ref:~100p", [Id, Ref]),
    Ref.

-spec send_some_msg_to_room(Pid :: pid()) -> ok.
send_some_msg_to_room(Pid) ->
    lager:debug("send_some_msg_to_room, Pid:~1000p", [Pid]),
    Body =
        #user_msg_to_room{
            body = <<"Message from bot">>,
            from = self()
        },
    Msg = chat_room_utils:make_msg(self(), Body),
    chat_room:cast(Pid, Msg).
