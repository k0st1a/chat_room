%%%-------------------------------------------------------------------
%% @doc chat_room top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_room_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => chat_room_manager,
            start => {chat_room_manager, start_link, []}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
