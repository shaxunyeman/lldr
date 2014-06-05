-module(lldr_echo_server).
-author("shaxunyeman@gmail.com").

-export([start/0]).
-export([acceptor/1]).
-export([acceptor_once/1]).

start() ->
  lldr_socket_server:start(?MODULE,8001,{?MODULE,acceptor}).

acceptor_once(Socket) ->  
  io:format("lldr_echo_server:acceptor_once ~n"),
  inet:setopts(Socket,[{active,once}]),
  %case gen_tcp:recv(Socket,0) of
  receive
	{tcp,Socket,Packet} ->
	  io:format("echo >> ~p ~n",[Packet]),
	  gen_tcp:send(Socket,Packet),
	  acceptor(Socket);
	{tcp_closed,Socket} ->
	  io:format("A client [~p ] has closed ~n",[Socket]),
	  ok
  end.


acceptor(Socket) ->
  %%error_logger:info_msg("~p:~p <~p> lldr_echo_server:acceptor ~n",[?MODULE,?LINE,self()]),
  case gen_tcp:recv(Socket,0) of
	{ok,Packet} ->
	  error_logger:info_msg("echo >> ~p ~n",[Packet]),
	  gen_tcp:send(Socket,Packet),
	  acceptor(Socket);
	{error,closed} ->
	  %case inet:peername(Socket) of
	  %	{ok,{Address,Port}} ->
	  %	  error_logger:info_msg("~p:~p <~p> A client [~p:~p] has closed ~n",[?MODULE,?LINE,self(),Address,Port]);
	  %	{error,Posix} ->	
	  %	  error_logger:info_msg("~p:~p <~p> get peer name failds,the reason is ~p ~n",[?MODULE,?LINE,self(),Posix])
	  %end,
	  error_logger:info_msg("~p:~p <~p> peer has closed ~n",[?MODULE,?LINE,self()]),
	  ok;
	{error,Reason} ->
	  case inet:peername(Socket) of
		{ok,{Address,Port}} ->
		  error_logger:info_msg("~p:~p <~p> A client [~p:~p ] has occours error,the reason is ~p ~n",[?MODULE,?LINE,self(),Address,Port,Reason]);
		{error,Posix} ->	
		  error_logger:info_msg("~p:~p <~p> get peer name failds,the reason is ~p ~n",[?MODULE,?LINE,self(),Posix])
	  end
  end.

