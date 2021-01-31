-module(chat_room).

-behaviour(gen_server).

-include("start_room.hrl").
-include("msg.hrl").
-include("chat_room_msg.hrl").
-include("chat_room_user_state.hrl").
-include("chat_room_bot_msg.hrl").

-export([
    %% API
    start_link/1,
    cast/2,
    call/2,
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
    users = #{} :: map() %% pid := user()
}).

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

%%--------------------------------------------------------------------
%% @doc
%% Cast message to this gen_server via pid.
%%
%% @spec cast(Pid :: pid(), Body :: term()) -> ok.
%% @end
%%--------------------------------------------------------------------
cast(Pid, Body) ->
    gen_server:cast(Pid, Body).

%%--------------------------------------------------------------------
%% @doc
%% Makes synchronous call to this gen_server via pid.
%%
%% @spec call(Pid :: pid(), Body :: term()) -> {ok, term()} | {error, term()}.
%% @end
%%--------------------------------------------------------------------
call(Pid, Body) ->
    gen_server:call(Pid, Body).

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
init(#start_room{with_bot = WithBot}) ->
    lager:info("Init, WithBot:~1000p", [WithBot]),
    case WithBot of
        true ->
            lager:info("Send self chat_room_bot", []),
            erlang:send(
                self(),
                #start_room_bot{bot_name = <<"Bot">>, room_pid = self()}
            );
        _ ->
            ok
    end,
    {ok, #state{}}.

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
%% Можно оптимизировать поиск бота, а именно, в #state{} добавить поле
%% bots значение которого будет мапа, где ключем будет выступать pid
%% процесса бота.
handle_call(#start_room_bot{} = Msg, _From, #state{users = Users} = State) ->
    lager:debug("start_room_bot, Msg:~1000000p, From: ~100p, Users:~n~p", [Msg, _From, Users]),
    Result = start_bot(Msg, Users),
    {reply, Result, State};
handle_call(#stop_room_bot{} = Msg, _From, #state{users = Users} = State) ->
    lager:debug("stop_room_bot, Msg:~1000000p, From: ~100p, Users:~n~p", [Msg, _From, Users]),
    case find_bot(Users) of
        #user{} = User->
            lager:debug("Bot found, User:~1000000p", [User]),
            Result = chat_room_sup:stop_room_bot(),
            lager:debug("Result:~1000p", [Result]),
            {reply, Result, State};
        _ ->
            lager:debug("Bot not found", []),
            {reply, error, State}
    end;
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
handle_cast(#msg{body = #user_enter_to_room{}} = Msg, #state{users = Users} = State) ->
    lager:debug("user_enter_to_room, Msg:~1000000p, Users:~n~p", [Msg, Users]),
    User = make_and_monitor_user(Msg),
    Users2 = add_user(User, Users),
    Notify = make_user_enter_to_room_notify(User),
    Resp = chat_room_utils:make_msg(self(), Notify),
    send(Resp, Users2),
    lager:debug("Users2:~n~p", [Users2]),
    {noreply, State#state{users = Users2}};
handle_cast(#msg{body = #user_msg_to_room{}} = Msg, #state{users = Users} = State) ->
    lager:debug("user_msg_to_room, Msg:~1000000p, Users:~n~p", [Msg, Users]),
    case find_user(Msg#msg.from, Users) of
        {ok, User} ->
            lager:debug("User found, User:~1000000p", [User]),
            Notify = make_user_msg_to_room_notify(Msg#msg.body, User),
            Resp = chat_room_utils:make_msg(self(), Notify),
            send(Resp, Users);
        _ ->
            lager:debug("User not found", [])
    end,
    {noreply, State};
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
handle_info(#start_room_bot{} = Msg, #state{users = Users} = State) ->
    lager:debug("info, start_room_bot, Msg:~1000p", [Msg]),
    start_bot(Msg, Users),
    {noreply, State};
handle_info({'DOWN', Ref, process, Pid, Reason}, #state{users = Users} = State) ->
    lager:debug("Process is down, Pid:~1000p, Ref:~1000p, Reason:~1000p", [Pid, Ref, Reason]),
    case take_user(Pid, Users) of
        {User, Users2} ->
            lager:debug("User found, User:~1000000p, Users2:~n ~p", [User, Users2]),
            Notify = make_user_leave_from_room_notify(User),
            Resp = chat_room_utils:make_msg(self(), Notify),
            lager:debug("Resp:~n~p", [Resp]),
            send(Resp, Users2),
            {noreply, State#state{users = Users2}};
        _ ->
            lager:debug("User not found", []),
            {noreply, State}
    end;
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
-spec make_and_monitor_user(Msg :: user_enter_to_room_notify()) -> User :: user().
make_and_monitor_user(Msg) ->
    lager:debug("make_and_monitor_user, Msg:~1000p", [Msg]),
    monitor_user(
        make_user(Msg)
    ).

-spec make_user(Msg :: user_enter_to_room()) -> User :: user().
make_user(#msg{body = #user_enter_to_room{} = Body} = Msg) ->
    lager:debug("make_user, Msg:~1000000p", [Msg]),
    #user{
        name = Body#user_enter_to_room.user,
        pid = Msg#msg.from,
        options =
            case Body#user_enter_to_room.is_bot of
                true ->
                    #{bot => true};
                _ ->
                    #{}
            end
    }.

-spec monitor_user(User :: user()) -> User2 :: user().
monitor_user(#user{} = User) ->
    User#user{
        ref = monitor(process, User#user.pid)
    }.

-spec make_user_enter_to_room_notify(User :: user()) -> user_enter_to_room_notify().
make_user_enter_to_room_notify(#user{} = User) ->
    #user_enter_to_room_notify{
        user_name = User#user.name,
        user_pid = User#user.pid
    }.

-spec make_user_leave_from_room_notify(User :: user()) -> user_leave_from_room_notify().
make_user_leave_from_room_notify(#user{} = User) ->
    #user_leave_from_room_notify{
        user_name = User#user.name,
        user_pid = User#user.pid
    }.

-spec make_user_msg_to_room_notify(user_msg_to_room(), user()) -> user_msg_to_room_notify().
make_user_msg_to_room_notify(#user_msg_to_room{} = Msg, #user{} = User) ->
    #user_msg_to_room_notify{
        user_name = User#user.name,
        user_pid = User#user.pid,
        msg_body = Msg#user_msg_to_room.body
    }.

-spec send(Msg :: term(), map() | list() | pid()) -> ok.
send(Msg, Pid) when is_pid(Pid) ->
    erlang:send(Pid, Msg),
    ok;
send(Msg, List) when is_list(List) ->
    lists:foreach(
        fun (Value) -> send(Msg, Value) end,
        List
    );
send(Msg, Users) when is_map(Users) ->
    %% т.к. в качестве ключа используется pid процесса
    send(Msg, maps:keys(Users)).

-spec add_user(User :: user(), Users :: map()) -> Users2 :: map().
add_user(#user{} = User, Users) ->
    maps:put(User#user.pid, User, Users).

-spec find_user(Pid :: pid(), Users :: map()) -> {ok, User :: user()} | false.
find_user(Pid, Users) ->
    maps:find(Pid, Users).

-spec take_user(Pid :: pid(), Users :: map()) -> {User :: user(), Users2 :: map()} | error.
take_user(Pid, Users) ->
    maps:take(Pid, Users).

-spec start_bot(Msg :: #start_room_bot{}, Users :: map()) -> {ok, pid()}
                                                           | {error, already_started}.
start_bot(#start_room_bot{} = Msg, Users) ->
    lager:debug("start_bot, Msg:~1000p, Users:~n~p", [Msg, users]),
    case find_bot(Users) of
        #user{} = User->
            lager:debug("Bot already started, User:~1000000p", [User]),
            {error, already_started};
        _ ->
            lager:debug("Bot not found => start bot", []),
            Result = chat_room_sup:start_room_bot(Msg),
            lager:debug("Result:~1000000p", [Result]),
            Result
    end.

-spec find_bot(map() | users() | user()) -> user() | false.
find_bot(Users) when is_map(Users) ->
    find_bot(maps:values(Users));
find_bot([User| Users]) ->
    case is_bot(User) of
        true ->
            User;
        _ ->
            find_bot(Users)
    end;
find_bot(_) ->
    false.

-spec is_bot(User :: user()) -> boolean().
is_bot(#user{options = #{bot := true}}) ->
    true;
is_bot(_) ->
    false.
