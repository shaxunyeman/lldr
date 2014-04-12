-module(lldr_data_store_SUITE).
-include_lib("C:/Program Files/erl5.10.4/lib/common_test-1.7.4/include/ct.hrl").
-include("../include/base.hrl").
-include("../include/lldr_data_store.hrl").
-compile(export_all).
-export([all/0]).
%%-export([init_per_testcase/2,end_per_testcase/2]).
-export([init_per_suite/1,end_per_suite/1]).
-export([test_wr/1]).
-export([test_get_password/1,test_update_password/1]).
-export([test_get_user_path/1]).


all() ->
  [test_wr,test_get_password,test_update_password,
  test_get_user_path].

init_per_suite(_Config) ->
  case whereis(lldr_data_store) of
  	undefined ->
  	  lldr_data_store:start() 
  end,
  lldr_data_store:add_user(#user{mail="rlq@eyou.net",sex=0,name="rlq",path="/home/data/rlq"},"3w.com"),
  lldr_data_store:add_user(#user{mail="rlq2@eyou.net",sex=0,name="rlq2",path="/home/data/rlq2"},"3w.com"),
  lldr_data_store:add_user(#user{mail="rlq3@eyou.net",sex=0,name="rlq3",path="/home/data/rlq3"},"3w.com"),
  _Config.

end_per_suite(_Config) ->
  lldr_data_store:stop().
  %unregister(lldr_data_store).

%%init_per_testcase(ets_test,_Config) ->
%%  [{key,"liuliu"}|_Config];
%%init_per_testcase(_,_Config) ->
%%  _Config.
  
%%end_per_testcase(ets_test,_Config) ->
%%  lists:keydelete(?config(key,_Config));
%%end_per_testcase(_,_Config) ->
%%  ok.

test_wr(_Config) ->
  User=#user{mail="liuliu@eyou.net",sex=1,name="liuliu",path="/home/data/liuliu"},
  lldr_data_store:add_user(User,"12306"),
  {ok,User} = lldr_data_store:get_user("liuliu@eyou.net"),
  io:format("~p ~n",[User]).

test_get_password(_Config) ->  
  {ok,"12306"} = lldr_data_store:get_user_password("liuliu@eyou.net").

test_update_password(_Config) ->  
  lldr_data_store:update_user_password({"liuliu@eyou.net","new12306"}),
  {ok,"new12306"} = lldr_data_store:get_user_password("liuliu@eyou.net").

test_get_user_path(_Config) ->
  {ok,"/home/data/liuliu"} = lldr_data_store:get_user_data_path("liuliu@eyou.net").
