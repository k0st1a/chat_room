-module(chat_room_manager).

-behaviour(gen_server).

-include("start_room.hrl").

-export([
    %% API
    start_link/0,
    start_room/0,
    start_room/1,
    %% gen_server callbacks
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    room :: undefined | pid(),
    ref :: undefined | reference(),
    bot_pid :: undefined | pid(),
    bot_ref :: undefined | pid()
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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Start room if not exists.
%%
%% @spec start_room() -> {ok, pid()}.
%% @end
%%--------------------------------------------------------------------
start_room() ->
    start_room(false).

%%--------------------------------------------------------------------
%% @doc
%% Start room if not exists.
%%
%% @spec start_room(WithBot :: boolean()) -> {ok, pid()}.
%% @end
%%--------------------------------------------------------------------
start_room(WithBot) ->
    gen_server:call(?MODULE, #start_room{with_bot = WithBot}).

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
handle_call(#start_room{}, From, #state{room = Pid} = State) when is_pid(Pid) ->
    lager:debug("start_room, room exists, Pid:~1000p, From:~100p", [Pid, From]),
    {reply, {ok, Pid}, State};
handle_call(#start_room{} = Msg, From, #state{} = State) ->
    lager:debug("start_room, room not exists, From:~100p", [From]),
    {ok, Pid} = chat_room_sup:start_room(Msg),
    Ref = monitor(process, Pid),
    lager:debug("Pid:~1000p, Ref:~1000p", [Pid, Ref]),
    {reply, {ok, Pid}, State#state{room = Pid, ref = Ref}};
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
handle_info({'DOWN', Ref, process, Pid, Reason}, #state{ref = Ref} = State) ->
    lager:debug("Room stopped, Pid:~1000p, Ref:~1000p, Reason:~1000p", [Pid, Ref, Reason]),
    {noreply, State#state{ref = undefined, room = undefined}};
%% unknown
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
