-module(protocol).
-include("base.hrl").

-define(LF,$\n).
-define(CR,$\r).
-define(ADDRCHR,$@).

-export([auth/1,response/2,binary_split/2]).

binary_split(B,C) ->
  binary_split(B,C,<<>>,[]).

binary_split(<<C,Rest/binary>>,C,Value,ValueList) ->
  binary_split(Rest,C,<<>>,[Value|ValueList]);
binary_split(<<C1,Rest/binary>>,C,Value,ValueList) ->
  binary_split(Rest,C,<<Value/binary,C1>>,ValueList);
binary_split(<<>>,_C,Value,ValueList) ->
  lists:reverse([Value|ValueList]).
  

auth(Binary) when is_binary(Binary) ->
  List = binary_split(Binary,?LF),
  %Client = #client{type='pc',version = binary_to_list(lists:nth(1,List)),
  %                  username = binary_to_list(lists:nth(2,List)),
  %                  password = binary_to_list(lists:nth(3,List))},
	NewList = [ list_to_tuple(string:tokens(Y,":")) || Y <- [binary_to_list(X) || X <- List]],
	%io:format("~p ~n",[NewList]),
	{?VERSION,Version} = lists:keyfind(?VERSION,1,NewList),
	{?USERNAME,UserName} = lists:keyfind(?USERNAME,1,NewList),
	{?PASSWORD,PassWord} = lists:keyfind(?PASSWORD,1,NewList),
	{?ID,Id} = lists:keyfind(?ID,1,NewList),

	Client = #client{type='pc',
										version=Version,
										username=UserName,
										password=PassWord},
	{ok,list_to_integer(Id),Client}.                  


response(EventId,Ret) ->
	Response = <<EventId:32,Ret:32>>,
	Response.





