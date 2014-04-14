-module(handle_protocol_SUITE).
-include_lib("C:/Program Files/erl5.10.4/lib/common_test-1.7.4/include/ct.hrl").
-include_lib("C:/Program Files/erl5.10.4/lib/kernel-2.16.4/include/file.hrl").
-include("../include/base.hrl").
-include("../include/lldr_data_store.hrl").
-compile(export_all).

-export([all/0]).
-export([init_per_suite/1,end_per_suite/1]).

-export([test_auth_ok/1,test_auth_failed/1,test_auth_user_not_exist/1]).
-export([test_post/1,test_post_data/1]).
-export([test_get_ok/1,test_get_file_not_exist/1]).
-export([test_push_data/1]).

init_per_suite(_Config) ->
  case whereis(lldr_data_store) of
	undefined ->
	  lldr_data_store:start()
  end,
  User = #user{mail="liuliu@eyou.net",sex=1,name="liuliu",path="E:/work/lldr/data/liuliu@eyou.net/"},
  lldr_data_store:add_user(User,"12306"),
  _Config.

end_per_suite(_Config) ->
  lldr_data_store:stop().
  %unregister(lldr_data_store).

all() ->
  [test_auth_ok,test_auth_failed,test_auth_user_not_exist,
  test_post,test_post_data,
  test_get_ok,test_get_file_not_exist,
  test_push_data].

test_auth_ok(_Config) ->
  Client = #client{type='pc',version='1.0.0',username="liuliu@eyou.net",password="12306"},
  ok = handle_protocol:auth(Client).
  
test_auth_failed(_Config) ->
  Client = #client{type='pc',version='1.0.0',username="liuliu@eyou.net",password="InvalidPassword"},
  {error,'password invalid'} = handle_protocol:auth(Client).

test_auth_user_not_exist(_Config) ->
  Client = #client{type='pc',version='1.0.0',username="liu@eyou.net",password="12306"},
  {error,'user not exist'} = handle_protocol:auth(Client).

test_post(_Config) ->
  Client = #client{type='pc',version='1.0.0',username="liuliu@eyou.net",password="12306"},
  Post = #post{id="2",crc="crc123",len="18",filename="test.txt",directory="doc"},
  {ok,{_Indentify,{file,IoDevice}}} = handle_protocol:post(Client,Post),
  file:close(IoDevice),
  FileName = "E:/work/lldr/data/liuliu@eyou.net/doc/test.txt",
  true = filelib:is_file(FileName).

test_post_data(_Config) -> 
  Client = #client{type='pc',version='1.0.0',username="liuliu@eyou.net",password="12306"},
  Post = #post{id="2",crc="crc123",len="20",filename="test_post_data.txt",directory="doc"},
  {ok,{Indentify,{file,IoDevice}}} = handle_protocol:post(Client,Post),
  PostData1 = #post_data{id="3",description=Indentify,data_begin="0",data_end="8",value="liuguozhu\n"},
  handle_protocol:post_data(IoDevice,PostData1),

  PostData2 = #post_data{id="3",description=Indentify,data_begin="10",data_end="18",value="liuguozhu\n"},
  handle_protocol:post_data(IoDevice,PostData2),
  file:close(IoDevice),

  FileName = "E:/work/lldr/data/liuliu@eyou.net/doc/test_post_data.txt",
  {ok,FileInfo} = file:read_file_info(FileName),
  20 = FileInfo#file_info.size.
  

test_get_ok(_Config) ->
  Client = #client{type='pc',version='1.0.0',username="liuliu@eyou.net",password="12306"},
  Get = #get{id="4",filename="doc/test_post_data.txt"},
  {ok,{_,{size,20}}} = handle_protocol:get(Client,Get).

test_get_file_not_exist(_Config) ->
  Client = #client{type='pc',version='1.0.0',username="liuliu@eyou.net",password="12306"},
  Get = #get{id="4",filename="doc/file_not_exits.txt"},
  {error,Reason} = handle_protocol:get(Client,Get),
  io:format("error : ~p ~n",[Reason]).

handle_data(Data,Size) when is_binary(Data) and is_integer(Size) ->
  io:format("Handle_Data ~p ~n",[binary_to_list(Data)]).
  %List = protocol:parse_binary(Data),
  %{ok,PushData} = protocol:push_data(List),
  %io:format("~n~p",[PushData#push_data.value]).

test_push_data(_Config) ->
  Client = #client{type='pc',version='1.0.0',username="liuliu@eyou.net",password="12306"},
  Get = #get{id="4",filename="doc/nginx.c"},
  Indentify = term_to_binary(make_ref()),
  handle_protocol:push_data(Client,Indentify,Get,{?MODULE,handle_data,[]}).














