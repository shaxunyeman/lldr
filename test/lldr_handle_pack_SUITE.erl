-module(lldr_handle_pack_SUITE).
-include_lib("C:/Program Files/erl5.10.4/lib/common_test-1.7.4/include/ct.hrl").
-include_lib("C:/Program Files/erl5.10.4/lib/kernel-2.16.4/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/base.hrl").
-include("../include/lldr_data_store.hrl").
-compile(export_all).

-export([all/0]).
-export([init_per_testcase/2,end_per_testcase/2]).
%-export([init_per_suite/1,end_per_suite/1]).

-export([test_handle_auth_ok/1,test_handle_auth_pwd_invalid/1,test_handle_auth_user_not_exist/1]).
-export([test_handle_post/1,test_handle_post_data/1]).
-export([test_handle_get/1]).
-export([test_handle_createdir/1,test_handle_deletedir/1,test_handle_modifydir/1,test_handle_listdir/1,test_handle_listdir_error/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_data_store() ->
  case whereis(lldr_data_store) of
  	undefined ->
  	  lldr_data_store:start_link(); 
	_Pid ->
	  void
  end,
  User = #user{mail="liuliu@eyou.net",sex=1,name="liuliu",path="E:/work/lldr/data/liuliu@eyou.net/"},
  lldr_data_store:add_user(User,"12306"),
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


all() ->
  %%[test_handle_auth_ok,test_handle_auth_pwd_invalid,test_handle_auth_user_not_exist,
  [test_handle_auth_ok,test_handle_auth_pwd_invalid,
	test_handle_post,test_handle_post_data,
	test_handle_get,test_handle_createdir,
	test_handle_deletedir,test_handle_modifydir,test_handle_listdir,
	test_handle_listdir_error
	].
  
test_handle_auth_ok(_Config) ->
  AuthBin = <<"{\"command\":\"auth\",\"id\":1,\"version\":\"1.0.0\",\"username\":\"liuliu@eyou.net\",\"password\":\"12306\"}">>,
  Status = [{auth,false}],
  {ok,Response,NewStatus} = lldr_handle_pack:parse_binary(AuthBin,Status),
  %% handle response
  {ok,Fields} = lldr_protocol_json:parse(Response),
  {ok,ResRecord} = lldr_protocol_json:normal_response(Fields),
  ?assertEqual(1,ResRecord#normal_response.id),
  ?assertEqual(?OK,ResRecord#normal_response.code),
  ?assertEqual({auth,true},lists:keyfind(auth,1,NewStatus)).

test_handle_auth_pwd_invalid(_Config) ->
  AuthBin = <<"{\"command\":\"auth\",\"id\":1,\"version\":\"1.0.0\",\"username\":\"liuliu@eyou.net\",\"password\":\"invalid_pwd\"}">>,
  Status = [{auth,false}],
  {error,Response,NewStatus} = lldr_handle_pack:parse_binary(AuthBin,Status),
  %% handle response
  {ok,Fields} = lldr_protocol_json:parse(Response),
  {ok,ResRecord} = lldr_protocol_json:normal_response(Fields),
  ?assertEqual(1,ResRecord#normal_response.id),
  ?assertEqual(?PWD_INVALID,ResRecord#normal_response.code),
  ?assertEqual({auth,false},lists:keyfind(auth,1,NewStatus)).

test_handle_auth_user_not_exist(_Config) ->
  AuthBin = <<"{\"command\":\"auth\",\"id\":1,\"version\":\"1.0.0\",\"username\":\"user_not_exist@eyou.net\",\"password\":\"invalid_pwd\"}">>,
  Status = [{auth,false}],
  {error,Response,NewStatus} = lldr_handle_pack:parse_binary(AuthBin,Status),
  %% handle response
  {ok,Fields} = lldr_protocol_json:parse(Response),
  {ok,ResRecord} = lldr_protocol_json:normal_response(Fields),
  ?assertEqual(1,ResRecord#normal_response.id),
  ?assertEqual(?USER_NOT_EXIST,ResRecord#normal_response.code),
  ?assertEqual({auth,false},lists:keyfind(auth,1,NewStatus)).

test_handle_post(_Config) ->

  PostBin = <<"{\"command\":\"post\",\"id\":2,\"crc\":\"1233dd\",\"length\":20,\"filename\":\"test_handle_post_data.txt\",\"directory\":\"doc\"}">>,
  %%{ok,PostFields} = lldr_protocol_json:parse(PostBin),
  %%{ok,Post} = lldr_protocol_json:post(PostFields),
  Client = #client{type='pc',version="1.0.0",username="liuliu@eyou.net",password="12306"},
  Status = [{auth,true},{client,Client}],
  {ok,Response,NewStatus} = lldr_handle_pack:parse_binary(PostBin,Status),
  io:format("NewStatus: ~p ~n",[NewStatus]),

  %% handle response
  {ok,Fields} = lldr_protocol_json:parse(Response),
  {ok,ResRecord} = lldr_protocol_json:post_response(Fields),
  io:format("ResRecord : ~p ~n",[ResRecord]),
  ?assertEqual(2,ResRecord#post_response.id),
  ?assertEqual(?OK,ResRecord#post_response.code),
  Indentify = ResRecord#post_response.filedescription,
  {Indentify,Value} = lists:keyfind(Indentify,1,NewStatus),
  ?assertEqual({cur_size,0},lists:keyfind(cur_size,1,Value)),
  {file_fd,IoDevice} = lists:keyfind(file_fd,1,Value),
  file:write(IoDevice,"this is a test"),
  file:close(IoDevice),
  
  {file,FullFileName} = lists:keyfind(file,1,Value),
  ?assertEqual(14,filelib:file_size(FullFileName)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-define(POST_FILE,"E:/work/ops/nginx/src/core/nginx.c").
-define(POST_FILE,"E:/work/lldr/data/liuliu@eyou.net/doc/IM.png").

handle_auth() ->
  AuthBin = <<"{\"command\":\"auth\",\"id\":1,\"version\":\"1.0.0\",\"username\":\"liuliu@eyou.net\",\"password\":\"12306\"}">>,
  Status = [{auth,false}],
  lldr_handle_pack:parse_binary(AuthBin,Status).

handle_post(Status) ->
  PostBin = <<"{\"command\":\"post\",\"id\":2,\"crc\":\"1233dd\",\"length\":36126,\"filename\":\"test_handle_post_data.png\",\"directory\":\"doc\"}">>,
  lldr_handle_pack:parse_binary(PostBin,Status).

generate_json_post_data(FileIndentify,Value,Begin,End) when is_integer(Begin) and is_integer(End) ->  
  Content = {struct,[{"command","postdata"},
					  {"filedescription",FileIndentify},
					  {"id",100},
					  {"begin",Begin},
					  {"end",End},
					  {"value",Value}]},
  Term_to_Json = mochijson2:encode(Content),
  Bin = list_to_binary(Term_to_Json),
  Bin.
  
test_handle_post_data(_Config) ->  
  {ok,AuthResponse,Status} = handle_auth(),
  {ok,AuthFields} = lldr_protocol_json:parse(AuthResponse),
  {ok,AuthResRecord} = lldr_protocol_json:normal_response(AuthFields),
  ?assertEqual(1,AuthResRecord#normal_response.id),
  ?assertEqual(?OK,AuthResRecord#normal_response.code),
  ?assertEqual({auth,true},lists:keyfind(auth,1,Status)),

  %% handle post request
  {ok,PostResponse,Post_NewStatus} = handle_post(Status),
  {ok,PostFields} = lldr_protocol_json:parse(PostResponse),
  {ok,PostRecord} = lldr_protocol_json:post_response(PostFields),
  ?assertEqual(2,PostRecord#post_response.id),
  ?assertEqual(?OK,PostRecord#post_response.code),
  FileDesc = PostRecord#post_response.filedescription,
  {FileDesc,ValueList} = lists:keyfind(FileDesc,1,Post_NewStatus),
  ?assertEqual({cur_size,0},lists:keyfind(cur_size,1,ValueList)),
  ?assertEqual({len,36126},lists:keyfind(len,1,ValueList)),
  %%{file_fd,IoDevice} = lists:keyfind(file_fd,1,ValueList),
  {file,FullFileName} = lists:keyfind(file,1,ValueList),
  %%file:close(IoDevice),
  %%io:format("~p:~p FullFileName is ~p and file's indentify is ~p ~n",[?MODULE,?LINE,FullFileName,FileDesc]),

  %%handle post data
  handle_io_input(FileDesc,FullFileName,Post_NewStatus),

  {ok,OriginData} = file:read_file(?POST_FILE), 
  {ok,NewData} = file:read_file(FullFileName), 
  ?assertEqual(erlang:crc32(OriginData),erlang:crc32(NewData)),
  ok.

handle_io_input(FileDesc,_FullFileName,_Status) ->
  %%{ok,IoInput} = file:open(FullFileName,[write,append]),
  {ok,IoOutput} = file:open(?POST_FILE,[read]),
  %%handle_io(FileDesc,IoOutput,IoInput,0).
  handle_net_io(FileDesc,IoOutput,0,_Status).

handle_io(FileDesc,IoOutput,IoInput,Location) ->
  case file:pread(IoOutput,Location,1024) of
	{ok,Data} ->
	  file:pwrite(IoInput,Location,Data),
	  NewLocation = Location + 1024,
	  handle_io(FileDesc,IoOutput,IoInput,NewLocation);
	eof  ->
	  file:close(IoInput),
	  file:close(IoOutput);
	{error,Reason} ->
	  io:format("~p : ~p read file occurs error. The reason is ~p ~n",[?MODULE,?LINE,Reason]) 
  end.

handle_net_io(FileDesc,IoOutput,Location,Status) ->
  case file:pread(IoOutput,Location,1024) of
	{ok,Data} ->
	  Begin = Location,
	  End = Location + 1024,
	  Json_Data = generate_json_post_data(FileDesc,Data,Begin,End - 1), 
	  %%io:format("~p ~n",[Json_Data]);
	  {ok,_Response,NewStatus} = lldr_handle_pack:parse_binary(Json_Data,Status),
	  %%io:format("~p : ~p Response is ~p ~n",[?MODULE,?LINE,Response]);
	  handle_net_io(FileDesc,IoOutput,End,NewStatus);
	eof ->
	  file:close(IoOutput);
	{error,Reason} ->
	  io:format("~p : ~p read file occurs error. The reason is ~p ~n",[?MODULE,?LINE,Reason]) 
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TEST_GET_FILE,"E:/work/lldr/data/liuliu@eyou.net/doc/test_handle_get.png").  

handle_output(Binary,IoDevice) ->
  {struct,List} = mochijson2:decode(Binary),
  case lists:keyfind(<<"code">>,1,List) of
	{<<"code">>,Code} ->
	  ?assertEqual(?OK,Code),
	  io:format("~p~n",[Binary]);
	false ->
	  {<<"begin">>,Begin} = lists:keyfind(<<"begin">>,1,List),
	  {<<"value">>,Value} = lists:keyfind(<<"value">>,1,List),
	  file:pwrite(IoDevice,Begin,Value)
  end.

  
test_handle_get(_Config) ->
  {ok,AuthResponse,Status} = handle_auth(),
  {ok,AuthFields} = lldr_protocol_json:parse(AuthResponse),
  {ok,AuthResRecord} = lldr_protocol_json:normal_response(AuthFields),
  ?assertEqual(1,AuthResRecord#normal_response.id),
  ?assertEqual(?OK,AuthResRecord#normal_response.code),
  ?assertEqual({auth,true},lists:keyfind(auth,1,Status)), 

  GetBin = <<"{\"command\":\"get\",\"id\":4,\"filename\":\"doc/IM.png\"}">>,
  {ok,IoDevice} = file:open(?TEST_GET_FILE,[write]),
  NewStatus = [{outputcb,{?MODULE,handle_output}},{socket,IoDevice}|Status],
  lldr_handle_pack:parse_binary(GetBin,NewStatus),
  file:close(IoDevice),

  {ok,Data} = file:read_file(?TEST_GET_FILE),
  ?assertEqual(3562928172,erlang:crc32(Data)),
  ok.

  
-define(PARENT_PATH,"E:/work/lldr/data/liuliu@eyou.net").
-define(EXPECT_PATH,"E:/work/lldr/data/liuliu@eyou.net/video/liudehua").
test_handle_createdir(_Config) ->
  {ok,AuthResponse,Status} = handle_auth(),
  {ok,AuthFields} = lldr_protocol_json:parse(AuthResponse),
  {ok,AuthResRecord} = lldr_protocol_json:normal_response(AuthFields),
  ?assertEqual(1,AuthResRecord#normal_response.id),
  ?assertEqual(?OK,AuthResRecord#normal_response.code),
  ?assertEqual({auth,true},lists:keyfind(auth,1,Status)),

  CreateDirBin = <<"{\"command\":\"createdir\",\"id\":4,\"parent\":\"E:/work/lldr/data/liuliu@eyou.net/\",\"directory\":\"video/liudehua\"}">>,
  case filelib:is_dir(?EXPECT_PATH) of
	true ->
	  file:del_dir(?EXPECT_PATH);
	false ->
	  void
  end,
  ?assertEqual(false,filelib:is_dir(?EXPECT_PATH)),
  {ok,Response,NewStatus} = lldr_handle_pack:parse_binary(CreateDirBin,Status),
  io:format("Response is ~p and Status is ~p ~n",[Response,NewStatus]),
  ?assertEqual(true,filelib:is_dir(?EXPECT_PATH)),
  {ok,NewStatus}.

test_handle_deletedir(_Config) ->
  %% After invoking, ?EXPECT_PATH must exist
  {ok,Status} = test_handle_createdir(_Config),
  ?assertEqual(true,filelib:is_dir(?EXPECT_PATH)),
    

  DelDirBin = <<"{\"command\":\"deletedir\",\"id\":4,\"directory\":\"video/liudehua\"}">>,
  {ok,Response,NewStatus} = lldr_handle_pack:parse_binary(DelDirBin,Status),
  io:format("Response is ~p and Status is ~p ~n",[Response,NewStatus]),
  ?assertEqual(false,filelib:is_dir(?EXPECT_PATH)),
  ok.

-define(NEW_EXPECT_PATH,"E:/work/lldr/data/liuliu@eyou.net/video/new_liudehua").
test_handle_modifydir(_Config) ->
  %% After invoking, ?EXPECT_PATH must exist
  {ok,Status} = test_handle_createdir(_Config),
  ?assertEqual(true,filelib:is_dir(?EXPECT_PATH)),

  ModifyDirBin = <<"{\"command\":\"modify\",\"id\":4,\"old\":\"video/liudehua\",\"new\":\"video/new_liudehua\"}">>,
  {ok,Response,NewStatus} = lldr_handle_pack:parse_binary(ModifyDirBin,Status),
  io:format("Response is ~p and Status is ~p ~n",[Response,NewStatus]),
  ?assertEqual(false,filelib:is_dir(?EXPECT_PATH)),
  ?assertEqual(true,filelib:is_dir(?NEW_EXPECT_PATH)),

  %% delete dir
  file:del_dir(?NEW_EXPECT_PATH),
  ?assertEqual(false,filelib:is_dir(?NEW_EXPECT_PATH)),
  ok.

test_handle_listdir(_Config) ->
  {ok,AuthResponse,Status} = handle_auth(),
  {ok,AuthFields} = lldr_protocol_json:parse(AuthResponse),
  {ok,AuthResRecord} = lldr_protocol_json:normal_response(AuthFields),
  ?assertEqual(1,AuthResRecord#normal_response.id),
  ?assertEqual(?OK,AuthResRecord#normal_response.code),
  ?assertEqual({auth,true},lists:keyfind(auth,1,Status)),

  ListDirBin = <<"{\"command\":\"listdir\",\"id\":4,\"directory\":\"doc\"}">>,
  {ok,Response,_NewStatus} = lldr_handle_pack:parse_binary(ListDirBin,Status),
  io:format("Response is ~p ~n",[Response]),
  ok.

test_handle_listdir_error(_Config) ->
  {ok,AuthResponse,Status} = handle_auth(),
  {ok,AuthFields} = lldr_protocol_json:parse(AuthResponse),
  {ok,AuthResRecord} = lldr_protocol_json:normal_response(AuthFields),
  ?assertEqual(1,AuthResRecord#normal_response.id),
  ?assertEqual(?OK,AuthResRecord#normal_response.code),
  ?assertEqual({auth,true},lists:keyfind(auth,1,Status)),

  ListDirBin = <<"{\"command\":\"listdir\",\"id\":4,\"directory\":\"doc2\"}">>,
  {error,Response,_NewStatus} = lldr_handle_pack:parse_binary(ListDirBin,Status),
  io:format("Response is ~p ~n",[Response]),
  ok.











