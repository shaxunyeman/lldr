-module(lldr_protocol_json).
-author("shaxunyeman@gmail.com").

%%-include("include/base.hrl").
-include("include/typedef.hrl").

-export([parse/1]).
-export([command/1,protocol_id/1]).
-export([auth/1,post/1,post_data/1,get/1,push_data/1]).
-export([createdir/1,deletedir/1,modifydir/1,listdir/1]).
-export([listfile/1,logout/1]).
-export([normal_response/1,post_response/1]).

%%-type(client() :: #client{}).
%%-type(post() :: #post{}).
%%-type(post_data() :: #post_data{}).
%%-type(push_data() :: #push_data{}).
%%-type(createdir() :: #createdir{}).
%%-type(deletedir() :: #deletedir{}).
%%-type(modifydir() :: #modify{}).
%%-type(listdir() :: #listdir{}).
%%-type(listfile() :: #listfile{}).
%%-type(normal_response() :: #normal_response{}).
%%-type(post_response() :: #post_response{}).
%%-type(get() :: #get{}).

-type(fields() :: [term()]).

-define(MTU,1496).

%%
%% 
%%
-spec(parse/1 ::(binary() | string()) -> {ok,fields()} | {error,any()}).
parse(ProtocolData) when is_binary(ProtocolData) or is_list(ProtocolData) ->
  case mochijson2:decode(ProtocolData) of
	{struct,Fields} ->
	  NewFields = [trans_key_value({Key,Value})|| {Key,Value} <- Fields],
	  {ok,NewFields};
	_Any ->
	  {error,_Any}
  end.

trans_key_value({Key,Value}) when is_binary(Key) and is_binary(Value) ->
  KeyStr = binary_to_list(Key),
  if 
	(KeyStr =:= "begin")  or (KeyStr =:= "end") or (KeyStr =:= "id") or (KeyStr =:= "code") ->
	  {KeyStr,binary_to_integer(Value)};
	(KeyStr =/= "begin") and (KeyStr =/= "end") and (KeyStr =/= "id") and (KeyStr =/= "code") ->
	  {KeyStr,binary_to_list(Value)}
  end;
trans_key_value({Key,Value}) when is_binary(Key) and is_list(Value) ->
  %%{binary_to_list(Key),Value};
  KeyStr = binary_to_list(Key),
  if 
	(KeyStr =:= "begin")  or (KeyStr =:= "end") or (KeyStr =:= "id") or (KeyStr =:= "code")->
	  {KeyStr,list_to_integer(Value)};
	(KeyStr =/= "begin") and (KeyStr =/= "end") and (KeyStr =/= "id") and (KeyStr =/= "code")->
	  {KeyStr,Value}
  end;
trans_key_value({Key,Value}) when is_binary(Key) and is_integer(Value) ->
  {binary_to_list(Key),Value};
trans_key_value({Key,Value}) when is_list(Key) and is_integer(Value) ->
  {Key,Value};
trans_key_value({Key,Value}) when is_list(Key) and is_binary(Value) ->
  if 
	(Key =:= "begin")  or (Key =:= "end") or (Key =:= "id") or (Key =:= "code")->
	  {Key,binary_to_integer(Value)};
	(Key =/= "begin") and (Key =/= "end") and (Key =/= "id") and (Key =/= "code")->
	  {Key,Value}
  end;
trans_key_value({Key,Value}) when is_list(Key) and is_list(Value) ->
  %% {Key,Value};
  if 
	(Key =:= "begin")  or (Key =:= "end") or (Key =:= "id") or (Key =:= "code")->
	  {Key,list_to_integer(Value)};
	(Key =/= "begin") and (Key =/= "end") and (Key =/= "id") and (Key =/= "code")->
	  {Key,Value}
  end.

-spec(command/1 :: (fields()) -> atom()).
command(Fields) when is_list(Fields) ->
  case lists:keyfind(?COMMAND,1,Fields) of
	{?COMMAND,Command} ->
	  Command;
	false ->
	  unknow
  end.

-spec(protocol_id/1 :: (fields()) -> integer() | invalid).
protocol_id(Fields) ->
  case lists:keyfind(?ID,1,Fields) of
	{?ID,Protocol_Id} ->
	  Protocol_Id;
	false ->
	  invalid
  end.

-spec(normal_response/1 :: (fields()) -> {ok,normal_response()} | {error,any()}).
normal_response(Fields) ->  
  {?ID,Id} = lists:keyfind(?ID,1,Fields),
  {?CODE,Code} = lists:keyfind(?CODE,1,Fields),
  Response = #normal_response{id=Id,code=Code},
  {ok,Response}.

-spec(post_response/1 :: (fields()) -> {ok,post_response()} | {error,any()}).
post_response(Fields) ->
  {?ID,Id} = lists:keyfind(?ID,1,Fields),
  {?CODE,Code} = lists:keyfind(?CODE,1,Fields),
  {?DESC,FileIndentify} = lists:keyfind(?DESC,1,Fields),
  Response = #post_response{id=Id,code=Code,filedescription=FileIndentify},
  {ok,Response}.
  
-spec(auth/1 :: (fields()) -> {ok,client()} | {error,any()}).
auth(Fields) ->
  {?VERSION,Version} = lists:keyfind(?VERSION,1,Fields),
  {?USERNAME,UserName} = lists:keyfind(?USERNAME,1,Fields),
  {?PASSWORD,PassWord} = lists:keyfind(?PASSWORD,1,Fields),
  %% {?ID,Id} = lists:keyfind(?ID,1,Fields),

  Client = #client{type='pc',
				  version=Version,
				  mtu = ?MTU,
				  username=UserName,
				  password=PassWord},
  {ok,Client}. 


-spec(post/1 :: (fields()) -> {ok,post()} | {error,any()}).  
post(Fields) ->  
  {?ID,Id} = lists:keyfind(?ID,1,Fields),
  {?CRC,Crc} = lists:keyfind(?CRC,1,Fields),
  {?POSTLENGTH,Len} = lists:keyfind(?POSTLENGTH,1,Fields),
  {?FILENAME,FileName} = lists:keyfind(?FILENAME,1,Fields),
  {?DIR,Dir} = lists:keyfind(?DIR,1,Fields),

  Post = #post{id=Id,crc=Crc,len=Len,filename=FileName,directory=Dir},
  {ok,Post}.

-spec(post_data/1 :: (fields()) -> {ok,post_data()} | {error,any()}).
post_data(Fields) ->
  {?ID,Id} = lists:keyfind(?ID,1,Fields),
  {?DESC,Description} = lists:keyfind(?DESC,1,Fields),
  {?POSTBEGIN,Begin} = lists:keyfind(?POSTBEGIN,1,Fields),
  {?POSTEND,End} = lists:keyfind(?POSTEND,1,Fields),
  {?POSTVALUE,Value} = lists:keyfind(?POSTVALUE,1,Fields),
  Post_Data = #post_data{id=Id,description=Description,
						data_begin=Begin,data_end=End,value=Value},
  {ok,Post_Data}.


-spec(get/1 :: (fields()) -> {ok,get()} | {error,any()}).  
get(Fields) ->
  {?ID,Id} = lists:keyfind(?ID,1,Fields),
  {?FILENAME,FileName} = lists:keyfind(?FILENAME,1,Fields),
  Get = #get{id=Id,filename=FileName},
  {ok,Get}.
  

-spec(push_data/1 :: (fields()) -> {ok,push_data()} | {error,any()}).
push_data(Fields) ->
  {?DESC,Desc} = lists:keyfind(?DESC,1,Fields),
  {?POSTBEGIN,Begin} = lists:keyfind(?POSTBEGIN,1,Fields),
  {?POSTEND,End} = lists:keyfind(?POSTEND,1,Fields),
  {?POSTVALUE,Value} = lists:keyfind(?POSTVALUE,1,Fields),
  PushData = #push_data{filedescription=Desc,data_begin=Begin,data_end=End,value=Value},
  {ok,PushData}.

-spec(createdir/1 :: (fields()) -> {ok,createdir()} | {error,any()}).  
createdir(Fields) ->
  {?ID,Id} = lists:keyfind(?ID,1,Fields),
  {?DIR,Dir} = lists:keyfind(?DIR,1,Fields),
  %%{?PARENT,Parent} = lists:keyfind(?PARENT,1,Fields),
  Directory = #createdir{id=Id,directory=Dir},
  {ok,Directory}.

-spec(deletedir/1 :: (fields()) -> {ok,deletedir()} | {error,any()}).
deletedir(Fields) ->
  {?ID,Id} = lists:keyfind(?ID,1,Fields),
  {?DIR,Dir} = lists:keyfind(?DIR,1,Fields),
  DeleteDir = #deletedir{id=Id,directory=Dir},
  {ok,DeleteDir}.

-spec(modifydir/1 :: (fields()) -> {ok,modifydir()} | {error,any()}).
modifydir(Fields) ->
  {?ID,Id} = lists:keyfind(?ID,1,Fields),
  {?MODIFYOLD,Old} = lists:keyfind(?MODIFYOLD,1,Fields),
  {?MODIFYNEW,New} = lists:keyfind(?MODIFYNEW,1,Fields),
  Modify = #modify{id=Id,old=Old,new=New},
  {ok,Modify}.

-spec(listdir/1 :: (fields()) -> {ok,listdir()} | {error,any()}).
listdir(Fields) ->
  {?ID,Id} = lists:keyfind(?ID,1,Fields),
  {?DIR,Dir} = lists:keyfind(?DIR,1,Fields),
  ListDir = #listdir{id=Id,directory=Dir},
  {ok,ListDir}.

-spec(listfile/1 :: (fields()) -> {ok,listfile()} | {error,any()}).
listfile(Fields) ->
  {?ID,Id} = lists:keyfind(?ID,1,Fields),
  {?DIR,Dir} = lists:keyfind(?DIR,1,Fields),
  ListFile = #listfile{id=Id,directory=Dir},
  {ok,ListFile}.


-spec(logout/1 :: (fields()) -> {ok,integer()} | {error,any()}).
logout(Fields) ->
  {?ID,Id} = lists:keyfind(?ID,1,Fields),
  {?TYPE,Type} = lists:keyfind(?TYPE,1,Fields),
  Logout = #logout{id=Id,type=Type},
  {ok,Logout}.












