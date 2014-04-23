-module(protocol).
-author("shaxunyeman@gmail.com").

-include("base.hrl").
-define(LF,$\n).  -define(CR,$\r).
-define(ADDRCHR,$@).

-export([auth/1,binary_split/2]).
-export([parse_binary/1,command/1]).
-export([post/1,post_data/1]).
-export([get/1]).
-export([push_data/1]).
-export([createdir/1,deletedir/1]).
-export([modify/1]).
-export([listdir/1,listfile/1]).
-export([logout/1]).


binary_split(B,C) ->
  binary_split(B,C,<<>>,[]).

binary_split(<<C,Rest/binary>>,C,Value,ValueList) ->
  binary_split(Rest,C,<<>>,[Value|ValueList]);
binary_split(<<C1,Rest/binary>>,C,Value,ValueList) ->
  binary_split(Rest,C,<<Value/binary,C1>>,ValueList);
binary_split(<<>>,_C,Value,ValueList) ->
  lists:reverse([Value|ValueList]).
  
parse_binary(Binary) when is_binary(Binary) ->
	List = binary_split(Binary,?LF),
	NewList = [ list_to_tuple(string:tokens(Y,":")) || Y <- [binary_to_list(X) || X <- List]],
	NewList.

auth(Binary) when is_binary(Binary) ->
  %%List = binary_split(Binary,?LF),
	%%NewList = [ list_to_tuple(string:tokens(Y,":")) || Y <- [binary_to_list(X) || X <- List]],
	%io:format("~p ~n",[NewList]),
	NewList = parse_binary(Binary),
	{?VERSION,Version} = lists:keyfind(?VERSION,1,NewList),
	{?USERNAME,UserName} = lists:keyfind(?USERNAME,1,NewList),
	{?PASSWORD,PassWord} = lists:keyfind(?PASSWORD,1,NewList),
	{?ID,Id} = lists:keyfind(?ID,1,NewList),

	Client = #client{type='pc',
										version=Version,
										mtu = 1496,
										username=UserName,
										password=PassWord},
	{ok,list_to_integer(Id),Client}.                  

command(List) when is_list(List) ->
	case lists:keyfind(?COMMAND,1,List) of
	  {?COMMAND,Command} ->
		  Command;
	  false ->
		  unknow  
	end.

	

post(List) when is_list(List) ->
	{?ID,Id} = lists:keyfind(?ID,1,List),
	{?CRC,Crc} = lists:keyfind(?CRC,1,List),
	{?POSTLENGTH,Len} = lists:keyfind(?POSTLENGTH,1,List),
	{?FILENAME,FileName} = lists:keyfind(?FILENAME,1,List),
	{?DIR,Dir} = lists:keyfind(?DIR,1,List),
	
	Post = #post{id=Id,crc=Crc,len=Len,filename=FileName,directory=Dir},
	{ok,Post}.

post_data(List) when is_list(List) ->
  {?ID,Id} = lists:keyfind(?ID,1,List),
  {?DESC,Description} = lists:keyfind(?DESC,1,List),
  {?POSTBEGIN,Begin} = lists:keyfind(?POSTBEGIN,1,List),
  {?POSTEND,End} = lists:keyfind(?POSTEND,1,List),
  {?POSTVALUE,Value} = lists:keyfind(?POSTVALUE,1,List),
  Post_Data = #post_data{id=Id,description=Description,
						data_begin=Begin,data_end=End,value=Value},
  {ok,Post_Data}.


get(List) when is_list(List) ->
  {?ID,Id} = lists:keyfind(?ID,1,List),
  {?FILENAME,FileName} = lists:keyfind(?FILENAME,1,List),
  Get = #get{id=Id,filename=FileName},
  {ok,Get}.

push_data(List) when is_list(List) ->
  io:format("~p ~n",[List]),
  {?DESC,Desc} = lists:keyfind(?DESC,1,List),
  {?POSTBEGIN,Begin} = lists:keyfind(?POSTBEGIN,1,List),
  {?POSTEND,End} = lists:keyfind(?POSTEND,1,List),
  {?POSTVALUE,Value} = lists:keyfind(?POSTVALUE,1,List),
  PushData = #push_data{filedescription=Desc,data_begin=Begin,data_end=End,value=Value},
  {ok,PushData}.

createdir(List) when is_list(List) ->
  {?ID,Id} = lists:keyfind(?ID,1,List),
  {?DIR,Dir} = lists:keyfind(?DIR,1,List),
  Directory = #createdir{id=Id,directory=Dir},
  {ok,Directory}.

deletedir(List) when is_list(List) ->
  {?ID,Id} = lists:keyfind(?ID,1,List),
  {?DIR,Dir} = lists:keyfind(?DIR,1,List),
  DeleteDir = #deletedir{id=Id,directory=Dir},
  {ok,DeleteDir}.


modify(List) when is_list(List) ->
  {?ID,Id} = lists:keyfind(?ID,1,List),
  {?MODIFYOLD,Old} = lists:keyfind(?MODIFYOLD,1,List),
  {?MODIFYNEW,New} = lists:keyfind(?MODIFYNEW,1,List),
  Modify = #modify{id=Id,old=Old,new=New},
  {ok,Modify}.

listdir(List) when is_list(List) ->
  {?ID,Id} = lists:keyfind(?ID,1,List),
  {?DIR,Dir} = lists:keyfind(?DIR,1,List),
  ListDir = #listdir{id=Id,directory=Dir},
  {ok,ListDir}.

listfile(List) when is_list(List) ->
  {?ID,Id} = lists:keyfind(?ID,1,List),
  {?DIR,Dir} = lists:keyfind(?DIR,1,List),
  ListFile = #listfile{id=Id,directory=Dir},
  {ok,ListFile}.


logout(List) when is_list(List) ->
  {?ID,Id} = lists:keyfind(?ID,1,List),
  {ok,list_to_integer(Id)}.



