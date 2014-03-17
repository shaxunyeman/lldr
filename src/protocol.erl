-module(protocol).
-include("base.hrl").

-define(LF,$\n).
-define(CR,$\r).
-define(ADDRCHR,$@).

-export([auth/1,response/2,binary_split/2]).
-export([parse_binary/1,command/1,post/1]).

binary_split(B,C) ->
  binary_split(B,C,<<>>,[]).

binary_split(<<C,Rest/binary>>,C,Value,ValueList) ->
  binary_split(Rest,C,<<>>,[Value|ValueList]);
binary_split(<<C1,Rest/binary>>,C,Value,ValueList) ->
  binary_split(Rest,C,<<Value/binary,C1>>,ValueList);
binary_split(<<>>,_C,Value,ValueList) ->
  lists:reverse([Value|ValueList]).
  
parse_binary(Binary) when is_binary(Binary) ->
	List = binary_split(Binary,?ADDRCHR),
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
										username=UserName,
										password=PassWord},
	{ok,list_to_integer(Id),Client}.                  

command(List) when is_list(List) ->
	{?COMMAND,Command} = lists:keyfind(?COMMAND,1,List),
	Command.

post(List) when is_list(List) ->
	{?ID,Id} = lists:keyfind(?ID,1,List),
	{?FILENAME,FileName} = lists:keyfind(?FILENAME,1,List),
	{?DIR,Dir} = lists:keyfind(?DIR,1,List),
	
	Post = #post{id=Id,filename=FileName,directory=Dir},
	{ok,Post}.

response(EventId,Ret) ->
	Response = <<EventId:32,Ret:32>>,
	Response.





