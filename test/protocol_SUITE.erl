-module(protocol_SUITE).
-include_lib("C:/Program Files/erl5.10.4/lib/common_test-1.7.4/include/ct.hrl").
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
  Auth = "command:auth\nid:1\nversion:1.0.0\nusername:liuliu@eyou.net\npassword:12306\n",
  AuthBin = list_to_binary(Auth),
  test_auth_impl(AuthBin).

test_auth_impl(Binary) when is_binary(Binary) ->
  {ok,1,_Client} = protocol:auth(Binary),
  #client{type='pc',version="1.0.0",username="liuliu@eyou.net",password="12306"} = _Client.

test_post(_Config) ->
  Post = "command:post\nid:2\ncrc:1233dd\nlength:9\nfilename:test.log\ndirectory:/home/data\n",
  Request = protocol:parse_binary(list_to_binary(Post)),
  ?POST = protocol:command(Request),
  {ok,PostRequest} = protocol:post(Request),
  #post{id="2",len="9",filename="test.log",directory="/home/data"} = PostRequest.

test_post_data(_Config) ->
  PostData = "command:postdata\nfiledescription:indentify\nid:3\nbegin:0\nend:8\nvalue:liuguozhu\n",
  Request = protocol:parse_binary(list_to_binary(PostData)),
  ?POSTDATA = protocol:command(Request),
  {ok,PostDataReqeust} = protocol:post_data(Request),
  #post_data{id="3",description="indentify",data_begin="0",data_end="8",value="liuguozhu"} = PostDataReqeust.


test_get(_Config) ->
  Get = "command:get\nid:4\nfilename:/home/data/test.txt\n",
  RequestGet = protocol:parse_binary(list_to_binary(Get)),
  ?GET = protocol:command(RequestGet),
  {ok,Request} = protocol:get(RequestGet),
  #get{id="4",filename="/home/data/test.txt"} = Request.


test_createdir(_Config) ->
  CreateDir = "command:createdir\nid:5\ndirectory:/home/data\n",
  RequestDir = protocol:parse_binary(list_to_binary(CreateDir)),
  ?MKDIR = protocol:command(RequestDir),
  {ok,Request} = protocol:createdir(RequestDir),
  #createdir{id="5",directory="/home/data"} = Request.

test_deletedir(_Config) ->
  DeleteDir = "command:deletedir\nid:6\ndirectory:/home/data\n",
  ReqDeleteDir = protocol:parse_binary(list_to_binary(DeleteDir)), 
  ?DELDIR = protocol:command(ReqDeleteDir),
  {ok,Request} = protocol:deletedir(ReqDeleteDir),
  #deletedir{id="6",directory="/home/data"} = Request.


test_modify(_Config) ->
  Modify = "command:modify\nid:7\nold:/home/data\nnew:/home/new_data\n",
  ReqModify = protocol:parse_binary(list_to_binary(Modify)),
  ?MODIFY = protocol:command(ReqModify),
  {ok,Request} = protocol:modify(ReqModify),
  #modify{id="7",old="/home/data",new="/home/new_data"} = Request.


test_listdir(_Config) ->
  ListDir = "command:listdir\nid:8\ndirectory:/home\n",
  ReqListDir = protocol:parse_binary(list_to_binary(ListDir)),
  ?LISTDIR = protocol:command(ReqListDir),
  {ok,Request} = protocol:listdir(ReqListDir),
  #listdir{id="8",directory="/home"} = Request.

test_listfile(_Config) ->
  ListFile = "command:listfile\nid:9\ndirectory:/home/data\n",
  ReqListFile = protocol:parse_binary(list_to_binary(ListFile)),
  ?LISTFILE = protocol:command(ReqListFile),
  {ok,Request} = protocol:listfile(ReqListFile),
  #listfile{id="9",directory="/home/data"} = Request.

test_logout(_Config) ->
  Logout = "command:logout\nid:10\n",
  ReqLogout = protocol:parse_binary(list_to_binary(Logout)),
  ?LOGOUT = protocol:command(ReqLogout),
  {ok,10} = protocol:logout(ReqLogout).

