-module(chat_room).

-behaviour(gen_server).

-include("chat_room_msg.hrl").
-include("chat_room_user_state.hrl").

-export([
    %% API
    start_link/0,
    cast/2,
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
    users = [] :: users()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Cast message to this gen_server via pid.
%%
%% @spec cast(Pid :: pid(), Body :: term()) -> ok.
%% @end
%%--------------------------------------------------------------------
cast(Pid, Body) ->
    gen_server:cast(Pid, Body).

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
init(Args) ->
    lager:info("Init, Args:~p", [Args]),
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
handle_cast(#user_enter_to_room{} = Msg, #state{users = Users} = State) ->
    lager:debug("user_enter_to_room, Msg:~1000000p, Users:~n~p", [Msg, Users]),
    User = make_and_monitor_user(Msg),
    Users2 = [User| Users],
    send(
        make_user_enter_to_room_notify(User),
        Users2
    ),
    lager:debug("Users2:~n~p", [Users2]),
    {noreply, State#state{users = Users2}};
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
handle_info({'DOWN', Ref, process, Pid, Reason}, #state{users = Users} = State) ->
    lager:debug("Process is down, Pid:~1000p, Ref:~1000p, Reason:~1000p", [Pid, Ref, Reason]),
    case lists:keytake(Ref, #user.ref, Users) of
        {_, User, Users2} ->
            lager:debug("User found, User:~1000000p, Users2:~n ~p", [User, Users2]),
            send(
                make_user_leave_from_room_notify(User),
                Users2
            ),
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
make_user(#user_enter_to_room{} = Msg) ->
    #user{
        name = Msg#user_enter_to_room.user,
        pid = Msg#user_enter_to_room.from
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

-spec send(Msg :: term(), User :: user() | users()) -> ok.
send(Msg, #user{pid = Pid}) ->
    erlang:send(Pid, Msg),
    ok;
send(Msg, Users) when is_list(Users) ->
    lists:foreach(
        fun (User) -> send(Msg, User) end,
        Users
    ).
