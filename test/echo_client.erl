-module(echo_client).
-author("db.liu").

-export([connect/1,close/1]).
-export([echo/2]).

connect({Ip,Port}) ->
  {ok,Socket} = gen_tcp:connect(Ip,Port,[binary,{packet,0},{active,false}]),
  Socket.

echo(Socket,Echo) ->
  gen_tcp:send(Socket,Echo),
  case gen_tcp:recv(Socket,0) of
	{ok,Packet} ->
	  io:format("echo << ~p ~n",[Packet]);
	{error,Reason} ->
	  io:format("error ~p ~n",[Reason])
  end.

close(Socket) ->
  gen_tcp:close(Socket).

