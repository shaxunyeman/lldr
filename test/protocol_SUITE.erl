-module(protocol_SUITE).
-include_lib("C:/Program Files (x86)/erl5.10.3/lib/common_test-1.7.3/include/ct.hrl").
-include("../include/base.hrl").
-compile(export_all).
-export([all/0]).
%%-export([init_per_testcase/2,end_per_testcase/2]).
-export([test_auth/1]).

all() -> [test_auth].

%%init_per_testcase(ets_test,_Config) ->
%%  [{key,"liuliu"}|_Config];
%%init_per_testcase(_,_Config) ->
%%  _Config.
  
%%end_per_testcase(ets_test,_Config) ->
%%  lists:keydelete(?config(key,_Config));
%%end_per_testcase(_,_Config) ->
%%  ok.
  
test_auth(_Config) ->
  Auth = "command:auth\nid:1\nversion:1.0.0\nusername:liuliu@eyou.net\npassword:12306\n",
  AuthBin = list_to_binary(Auth),
  test_auth_impl(AuthBin).

test_auth_impl(Binary) when is_binary(Binary) ->
  {ok,1,_Client} = protocol:auth(Binary).
