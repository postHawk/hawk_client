%%%-------------------------------------------------------------------
%%% @author Maximilian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Янв. 2016 18:58
%%%-------------------------------------------------------------------
-module(basic_SUITE).
-author("Maximilian").

-compile(export_all).

-include ("../include/env.hrl").
-include ("../include/mac.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_suite(Config) ->

	code:add_patha("/home/admin/test_hawk_client/ebin/"),
	code:add_patha("/home/admin/test_hawk_client/deps/bson/ebin/"),
	code:add_patha("/home/admin/test_hawk_client/deps/ranch/ebin/"),
	code:add_patha("/home/admin/test_hawk_client/deps/mongodb/ebin/"),
	code:add_patha("/home/admin/test_hawk_client/deps/jsx/ebin/"),
	code:add_patha("/home/admin/test_hawk_client/deps/gproc/ebin/"),
	code:add_patha("/home/admin/test_hawk_client/deps/gun/ebin/"),
	code:add_patha("/home/admin/test_hawk_client/deps/cowlib/ebin/"),
	code:add_patha("/home/admin/test_hawk_client/deps/shotgun/ebin/"),

	ok = application:ensure_started(crypto),
	ok = application:ensure_started(ranch),
	ok = application:ensure_started(gproc),
	ok = application:ensure_started(inets),

	application:ensure_all_started(shotgun),

	ok = application:ensure_started (hawk_client),
	Config.

end_per_suite(Config) ->
	application:stop(crypto),
	application:stop(ranch),
	application:stop(gproc),
	application:stop(inets),
	application:stop(shotgun),
	application:stop(hawk_client),
	Config.

all() ->
    [
        call_test
    ].

call_test(_Config) ->
    {ok, Conn} = shotgun:open("post-hawk.com", ?DEF_PORT, https),
    {ok, Response} = shotgun:get(Conn, "/", #{
    	<<"Origin">> => <<"https://test.local">>,
    	<<"Sec-WebSocket-Key">> => <<"IsoYCcUHGPU/+m1A72/Mxw==">>
    }),

    Headers = maps:get(headers, Response),
    Key = proplists:get_value(<<"WebSocket-Accept">>, Headers),
    ?assertEqual(<<"Cv69Tz3lk8p3p6vGupJIE+pS7ws=">>, Key).
