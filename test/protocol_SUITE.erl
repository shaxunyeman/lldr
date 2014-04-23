-module(protocol_SUITE).
-include_lib("C:/Program Files/erl5.10.4/lib/common_test-1.7.4/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/base.hrl").
-compile(export_all).
-export([all/0]).
%%-export([init_per_testcase/2,end_per_testcase/2]).
-export([test_auth/1,test_post/1,test_post_data/1,test_get/1]).
-export([test_createdir/1,test_deletedir/1]).
-export([test_modify/1,test_listdir/1,test_listfile/1]).
-export([test_logout/1]).

all() -> [test_auth,test_post,test_post_data,test_get,
		  test_createdir,test_deletedir,test_modify,
		  test_listdir,test_listfile,test_logout].

%%init_per_testcase(ets_test,_Config) ->
%%  [{key,"liuliu"}|_Config];
%%init_per_testcase(_,_Config) ->
%%  _Config.
  
%%end_per_testcase(ets_test,_Config) ->
%%  lists:keydelete(?config(key,_Config));
%%end_per_testcase(_,_Config) ->
%%  ok.
  
test_auth(_Config) ->
  AuthBin = <<"{\"command\":\"auth\",\"id\":1,\"version\":\"1.0.0\",\"username\":\"liuliu@eyou.net\",\"password\":\"12306\"}">>,
  test_auth_impl(AuthBin).

test_auth_impl(Binary) when is_binary(Binary) ->
  {ok,Fields} = lldr_protocol_json:parse(Binary),
  ?assertEqual("auth",lldr_protocol_json:command(Fields)), 
  io:format("~p ~n",[Fields]),
  {ok,Client} = lldr_protocol_json:auth(Fields),
  ?assertEqual("liuliu@eyou.net",Client#client.username),
  ?assertEqual("12306",Client#client.password),
  ?assertEqual("1.0.0",Client#client.version),
  ?assertEqual(1,lldr_protocol_json:protocol_id(Fields)).

test_post(_Config) ->
  Post= <<"{\"command\":\"post\",\"id\":2,\"crc\":\"1233dd\",\"length\":9,\"filename\":\"test.log\",\"directory\":\"/home/data\"}">>,
  {ok,Fields} = lldr_protocol_json:parse(Post),
  ?assertEqual("post",lldr_protocol_json:command(Fields)),
  {ok,PostRequest} = lldr_protocol_json:post(Fields),
  ?assertEqual(2,lldr_protocol_json:protocol_id(Fields)),
  ?assertEqual("1233dd",PostRequest#post.crc),
  ?assertEqual(9,PostRequest#post.len),
  ?assertEqual("test.log",PostRequest#post.filename),
  ?assertEqual("/home/data",PostRequest#post.directory).

test_post_data(_Config) ->
  PostData = <<"{\"command\":\"postdata\",\"filedescription\":\"indentify\",\"id\":3,\"begin\":0,\"end\":11,\"value\":\"liuguozhu\r\nrenliqiong\"}">>,
  {ok,Fields} = lldr_protocol_json:parse(PostData),
  ?assertEqual(?POSTDATA,lldr_protocol_json:command(Fields)),
  {ok,PostDataRequest} = lldr_protocol_json:post_data(Fields),
  ?assertEqual("indentify",PostDataRequest#post_data.description),
  ?assertEqual(3,PostDataRequest#post_data.id),
  ?assertEqual(0,PostDataRequest#post_data.data_begin),
  ?assertEqual(11,PostDataRequest#post_data.data_end),
  ?assertEqual("liuguozhu\r\nrenliqiong",PostDataRequest#post_data.value).


test_get(_Config) ->
  Get = "{\"command\":\"get\",\"id\":4,\"filename\":\"/home/data/test.txt\"}",
  {ok,Fields} = lldr_protocol_json:parse(Get),
  ?assertEqual(?GET,lldr_protocol_json:command(Fields)),
  {ok,GetRequest} = lldr_protocol_json:get(Fields),
  ?assertEqual(4,GetRequest#get.id),
  ?assertEqual("/home/data/test.txt",GetRequest#get.filename).


test_createdir(_Config) ->
  CreateDir = "{\"command\":\"createdir\",\"id\":5,\"directory\":\"/home/data\"}",
  {ok,Fields} = lldr_protocol_json:parse(CreateDir),
  ?assertEqual(?MKDIR,lldr_protocol_json:command(Fields)),
  {ok,Request} = lldr_protocol_json:createdir(Fields),
  ?assertEqual(5,Request#createdir.id),
  ?assertEqual("/home/data",Request#createdir.directory).

test_deletedir(_Config) ->
  DeleteDir = "{\"command\":\"deletedir\",\"id\":6,\"directory\":\"/home/data\"}",
  {ok,Fields} = lldr_protocol_json:parse(DeleteDir), 
  ?assertEqual(?DELDIR,lldr_protocol_json:command(Fields)),
  {ok,Request} = lldr_protocol_json:deletedir(Fields),
  ?assertEqual(6,Request#deletedir.id),
  ?assertEqual("/home/data",Request#deletedir.directory).


test_modify(_Config) ->
  Modify = "{\"command\":\"modify\",\"id\":7,\"old\":\"/home/data\",\"new\":\"/home/new_data\"}",
  {ok,Fields} = lldr_protocol_json:parse(Modify),
  ?assertEqual(?MODIFY,lldr_protocol_json:command(Fields)),
  {ok,Request} = lldr_protocol_json:modifydir(Fields),
  ?assertEqual(7,Request#modify.id),
  ?assertEqual("/home/data",Request#modify.old),
  ?assertEqual("/home/new_data",Request#modify.new).


test_listdir(_Config) ->
  ListDir = "{\"command\":\"listdir\",\"id\":8,\"directory\":\"/home\"}",
  {ok,Fields} = lldr_protocol_json:parse(ListDir),
  ?assertEqual(?LISTDIR,lldr_protocol_json:command(Fields)),
  {ok,Request} = lldr_protocol_json:listdir(Fields),
  ?assertEqual(8,Request#listdir.id),
  ?assertEqual("/home",Request#listdir.directory).

test_listfile(_Config) ->
  ListFile = "{\"command\":\"listfile\",\"id\":9,\"directory\":\"/home/data\"}",
  {ok,Fields} = lldr_protocol_json:parse(ListFile),
  ?assertEqual(?LISTFILE,lldr_protocol_json:command(Fields)),
  {ok,Request} = lldr_protocol_json:listfile(Fields),
  ?assertEqual(9,Request#listfile.id),
  ?assertEqual("/home/data",Request#listfile.directory).

test_logout(_Config) ->
  Logout = "{\"command\":\"logout\",\"id\":10,\"type\":\"pc\"}",
  {ok,Fields} = lldr_protocol_json:parse(Logout),
  ?assertEqual(?LOGOUT,lldr_protocol_json:command(Fields)),
  {ok,LogOut} = lldr_protocol_json:logout(Fields),
  ?assertEqual(10,LogOut#logout.id),
  ?assertEqual("pc",LogOut#logout.type).












