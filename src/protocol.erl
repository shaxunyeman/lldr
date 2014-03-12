-module(protocol).
-include("base.hrl").

-export([auth/1,binary_split/2]).

binary_split(B,C) ->
  binary_split(B,C,<<>>,[]).

binary_split(<<C,Rest/binary>>,C,Value,ValueList) ->
  binary_split(Rest,C,<<>>,[Value|ValueList]);
binary_split(<<C1,Rest/binary>>,C,Value,ValueList) ->
  binary_split(Rest,C,<<Value/binary,C1>>,ValueList);
binary_split(<<>>,_C,Value,ValueList) ->
  lists:reverse([Value|ValueList]).
  

auth(Binary) when is_binary(Binary) ->
  List = binary_split(Binary,$\n),
  Client = #client{type='pc',version = binary_to_list(lists:nth(1,List)),
                    username = binary_to_list(lists:nth(2,List)),
                    password = binary_to_list(lists:nth(3,List))},

  io:format("~p ~n",[Client#client.username]),                  
  Client.                  
