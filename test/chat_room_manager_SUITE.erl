-module(chat_room_manager_SUITE).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([
    all/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    start_room/1,
    start_and_stop_room/1
]).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() -> [
    start_room,
    start_and_stop_room
].

suite() ->
    [{timetrap, {seconds, 5}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    chat_room_api:lager(),
    chat_room_api:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    chat_room_api:stop(),
    chat_room_api:nolager(),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================
start_room(_Config) ->
    {ok, Pid} = chat_room_manager:start_room(),
    {ok, Pid2} = chat_room_manager:start_room(),
    ?assertEqual(Pid, Pid2).

start_and_stop_room(_Config) ->
    {ok, Pid} = chat_room_manager:start_room(),
    chat_room_sup:stop_room(),
    {ok, Pid2} = chat_room_manager:start_room(),
    ?assertNotEqual(Pid, Pid2),
    {ok, Pid3} = chat_room_manager:start_room(),
    ?assertEqual(Pid2, Pid3).
