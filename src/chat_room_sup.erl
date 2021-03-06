%%%-------------------------------------------------------------------
%% @doc chat_room top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_room_sup).

-behaviour(supervisor).

-export([
    %% API
    start_link/0,
    start_room/1,
    stop_room/0, %% for test only
    start_room_bot/1,
    stop_room_bot/0, %% for test only
    %% Supervisor callbacks
    init/1
]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one
    },
    ChildSpecs = [
        #{
            id => chat_room_manager,
            start => {chat_room_manager, start_link, []}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

start_room(Args) ->
    Spec = #{
        id => chat_room,
		start => {chat_room, start_link, [Args]},
		restart => temporary
    },
    supervisor:start_child(?MODULE, Spec).

stop_room() ->
    supervisor:terminate_child(?MODULE, chat_room).

start_room_bot(Args) ->
    Spec = #{
        id => chat_room_bot,
		start => {chat_room_bot, start_link, [Args]},
		restart => temporary
    },
    supervisor:start_child(?MODULE, Spec).

stop_room_bot() ->
    supervisor:terminate_child(?MODULE, chat_room_bot).
