-module(lldr_data_store_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/base.hrl").
-include("../include/lldr_data_store.hrl").

-compile(export_all).
-export([all/0]).
-export([init_per_testcase/2,end_per_testcase/2]).
%%-export([init_per_suite/1,end_per_suite/1]).
-export([test_wr/1]).
-export([test_get_password/1,test_update_password/1]).
-export([test_get_user_path/1]).


all() ->
  [test_wr,test_get_password,test_update_password,
  test_get_user_path].
  %[test_wr].

init_data_store() ->
  case whereis(lldr_data_store) of
  	undefined ->
  	  lldr_data_store:start_link(); 
	_Pid ->
	  void
  end,
  lldr_data_store:add_user(#user{mail="rlq@eyou.net",sex=0,name="rlq",path="/home/data/rlq"},"3w.com"),
  lldr_data_store:add_user(#user{mail="rlq2@eyou.net",sex=0,name="rlq2",path="/home/data/rlq2"},"3w.com"),
  lldr_data_store:add_user(#user{mail="rlq3@eyou.net",sex=0,name="rlq3",path="/home/data/rlq3"},"3w.com"),
  ok.

uninit_data_store() ->
  lldr_data_store:stop().

%init_per_suite(_Config) ->
%  init_data_store(),
%  _Config.

%end_per_suite(_Config) ->
%  uninit_data_store().

init_per_testcase(_TestCase,_Config) ->
  init_data_store(),
  _Config.
  
end_per_testcase(_TestCase,_Config) ->
  uninit_data_store().

test_wr(_Config) ->
  User=#user{mail="liuliu@eyou.net",sex=1,name="liuliu",path="/home/data/liuliu"},
  lldr_data_store:add_user(User,"12306"),
  ?assertEqual({ok,User},lldr_data_store:get_user("liuliu@eyou.net")),
  io:format("~p ~n",[User]).

test_get_password(_Config) ->  
  ?assertEqual({ok,"3w.com"},lldr_data_store:get_user_password("rlq@eyou.net")).

test_update_password(_Config) ->  
  ?assertEqual({ok,"3w.com"},lldr_data_store:get_user_password("rlq@eyou.net")),
  lldr_data_store:update_user_password({"rlq@eyou.net","new12306"}),
  ?assertEqual({ok,"new12306"},lldr_data_store:get_user_password("rlq@eyou.net")).

test_get_user_path(_Config) ->
  ?assertEqual({ok,"/home/data/rlq"},lldr_data_store:get_user_data_path("rlq@eyou.net")).
