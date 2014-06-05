-module(echo_client).
-author("shaxunyeman@gmail.com").

-export([connect/1,echo/2,close/1]).


connect({Host,Port}) when is_list(Host) and is_integer(Port) ->
  case gen_tcp:connect(Host,Port,[binary,{packet,0},{active,false}]) of
	{ok,Socket} ->
	  io:format("~p:~p <~p> connect ~p:~p successful ~n",[?MODULE,?LINE,self(),Host,Port]),
	  Socket;
	{error,Reason} ->
	  io:format("~p:~p <~p> connect ~p:~p failed,the reason is ~p ~n",[?MODULE,?LINE,self(),Host,Port,Reason]),
	  error
  end.


echo(Socket,Data) ->  
  case gen_tcp:send(Socket,Data) of
	ok ->
	  io:format("~p:~p <~p> echo >> ~p ~n",[?MODULE,?LINE,self(),Data]),
	  case  gen_tcp:recv(Socket,0,5000) of
		{ok,Packet} ->
		  io:format("~p:~p <~p> echo << ~p ~n",[?MODULE,?LINE,self(),binary_to_list(Packet)])
	  end;
	{error,Reason} ->
	  io:format("~p:~p <~p> send data failed,the reason is ~p ~n",[?MODULE,?LINE,self(),Reason]),
	  gen_tcp:close(Socket)
  end.

close(Socket) ->
  gen_tcp:close(Socket).
