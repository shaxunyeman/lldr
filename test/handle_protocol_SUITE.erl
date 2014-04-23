-module(handle_protocol_SUITE).
-include_lib("C:/Program Files/erl5.10.4/lib/common_test-1.7.4/include/ct.hrl").
-include_lib("C:/Program Files/erl5.10.4/lib/kernel-2.16.4/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
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
  AuthBin = <<"{\"command\":\"auth\",\"id\":1,\"version\":\"1.0.0\",\"username\":\"liuliu@eyou.net\",\"password\":\"12306\"}">>,
  {ok,Fields} = lldr_protocol_json:parse(AuthBin),
  {ok,Client} = lldr_protocol_json:auth(Fields),
  ?assertEqual(ok,handle_protocol:auth(Client)).
  
test_auth_failed(_Config) ->
  AuthBin = <<"{\"command\":\"auth\",\"id\":1,\"version\":\"1.0.0\",\"username\":\"liuliu@eyou.net\",\"password\":\"Invalidpassword\"}">>,
  {ok,Fields} = lldr_protocol_json:parse(AuthBin),
  {ok,Client} = lldr_protocol_json:auth(Fields),
  ?assertEqual({error,'password invalid'},handle_protocol:auth(Client)).

test_auth_user_not_exist(_Config) ->
  %% Client = #client{type='pc',version='1.0.0',username="liu@eyou.net",password="12306"},
  AuthBin = <<"{\"command\":\"auth\",\"id\":1,\"version\":\"1.0.0\",\"username\":\"user_not_exist@eyou.net\",\"password\":\"Invalidpassword\"}">>,
  {ok,Fields} = lldr_protocol_json:parse(AuthBin),
  {ok,Client} = lldr_protocol_json:auth(Fields),
  ?assertEqual({error,'user not exist'},handle_protocol:auth(Client)).

test_post(_Config) ->
  Client = #client{type='pc',version='1.0.0',username="liuliu@eyou.net",password="12306"},
  Post = #post{id="2",crc="crc123",len="18",filename="test.txt",directory="doc"},
  {ok,{_Indentify,{file,IoDevice}}} = handle_protocol:post(Client,Post),
  file:close(IoDevice),
  FileName = "E:/work/lldr/data/liuliu@eyou.net/doc/test.txt",
  true = filelib:is_file(FileName).

test_post_data(_Config) -> 
  %% client
  AuthBin = <<"{\"command\":\"auth\",\"id\":1,\"version\":\"1.0.0\",\"username\":\"liuliu@eyou.net\",\"password\":\"12306\"}">>,
  {ok,Fields} = lldr_protocol_json:parse(AuthBin),
  {ok,Client} = lldr_protocol_json:auth(Fields),
  
  %% post
  PostBin = <<"{\"command\":\"post\",\"id\":2,\"crc\":\"1233dd\",\"length\":20,\"filename\":\"test_post_data.txt\",\"directory\":\"doc\"}">>,
  {ok,PostFields} = lldr_protocol_json:parse(PostBin),
  {ok,Post} = lldr_protocol_json:post(PostFields),

  {ok,{_Indentify,{file,IoDevice}}} = handle_protocol:post(Client,Post),

  %% post_data
  PostDataBin = <<"{\"command\":\"postdata\",\"filedescription\":\"indentify\",\"id\":3,\"begin\":0,\"end\":9,\"value\":\"liuguozhu\n\"}">>,
  {ok,Fields1} = lldr_protocol_json:parse(PostDataBin),
  {ok,PostData1} = lldr_protocol_json:post_data(Fields1),
  handle_protocol:post_data(IoDevice,PostData1),

  PostDataBin2 = <<"{\"command\":\"postdata\",\"filedescription\":\"indentify\",\"id\":4,\"begin\":10,\"end\":19,\"value\":\"liuguozhu\n\"}">>,
  {ok,Fields2} = lldr_protocol_json:parse(PostDataBin2),
  {ok,PostData2} = lldr_protocol_json:post_data(Fields2),
  handle_protocol:post_data(IoDevice,PostData2),
  file:close(IoDevice),

  FileName = "E:/work/lldr/data/liuliu@eyou.net/doc/test_post_data.txt",
  {ok,FileInfo} = file:read_file_info(FileName),
  ?assertEqual(20,FileInfo#file_info.size).
  

test_get_ok(_Config) ->
  %% Client = #client{type='pc',version='1.0.0',username="liuliu@eyou.net",password="12306"},
  %% client
  AuthBin = <<"{\"command\":\"auth\",\"id\":1,\"version\":\"1.0.0\",\"username\":\"liuliu@eyou.net\",\"password\":\"12306\"}">>,
  {ok,ClientFields} = lldr_protocol_json:parse(AuthBin),
  {ok,Client} = lldr_protocol_json:auth(ClientFields),

  %% Get = #get{id="4",filename="doc/test_post_data.txt"},
  GetTerm = "{\"command\":\"get\",\"id\":4,\"filename\":\"doc/test_post_data.txt\"}",
  {ok,GetFields} = lldr_protocol_json:parse(GetTerm),
  {ok,GetRequest} = lldr_protocol_json:get(GetFields),

  {ok,{_,{size,20}}} = handle_protocol:get(Client,GetRequest).

test_get_file_not_exist(_Config) ->
  %% Client = #client{type='pc',version='1.0.0',username="liuliu@eyou.net",password="12306"},
  %% Get = #get{id="4",filename="doc/file_not_exits.txt"},

  AuthBin = <<"{\"command\":\"auth\",\"id\":1,\"version\":\"1.0.0\",\"username\":\"liuliu@eyou.net\",\"password\":\"12306\"}">>,
  {ok,ClientFields} = lldr_protocol_json:parse(AuthBin),
  {ok,Client} = lldr_protocol_json:auth(ClientFields),

  %% Get = #get{id="4",filename="doc/test_post_data.txt"},
  GetTerm = "{\"command\":\"get\",\"id\":4,\"filename\":\"doc/file_not_exits.txt\"}",
  {ok,GetFields} = lldr_protocol_json:parse(GetTerm),
  {ok,GetRequest} = lldr_protocol_json:get(GetFields),

  ?assertEqual({error,'file not exist'},handle_protocol:get(Client,GetRequest)).
  %% {error,Reason} = handle_protocol:get(Client,GetRequest),
  %% io:format("error : ~p ~n",[Reason]).

handle_push_data(JsonData,Io) ->
  %% io:format("~p",[JsonData]),
  {ok,Fields} = lldr_protocol_json:parse(JsonData),
  {ok,PushData} = lldr_protocol_json:push_data(Fields),
  file:pwrite(Io,PushData#push_data.data_begin,PushData#push_data.value).
  

test_push_data(_Config) ->
  %% Client = #client{type='pc',version='1.0.0',username="liuliu@eyou.net",password="12306"},
  AuthBin = <<"{\"command\":\"auth\",\"id\":1,\"version\":\"1.0.0\",\"username\":\"liuliu@eyou.net\",\"password\":\"12306\"}">>,
  {ok,ClientFields} = lldr_protocol_json:parse(AuthBin),
  {ok,Client} = lldr_protocol_json:auth(ClientFields),

  %% Get = #get{id="4",filename="doc/nginx.c"},
  GetTerm = "{\"command\":\"get\",\"id\":4,\"filename\":\"doc/IM.png\"}",
  {ok,GetFields} = lldr_protocol_json:parse(GetTerm),
  {ok,GetRequest} = lldr_protocol_json:get(GetFields),

  Indentify = term_to_binary(make_ref()),
  {ok,Fd} = file:open("E:/work/lldr/data/liuliu@eyou.net/doc/rewrit_IM.png",[write,binary]),
  State = [{socket,Fd}],
  handle_protocol:push_data(Client,Indentify,GetRequest,State,{?MODULE,handle_push_data}),
  file:close(Fd).













